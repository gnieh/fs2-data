/*
 * Copyright 2022 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fs2.data.json

import ast._
import codec._

import cats.data.NonEmptyList
import cats.syntax.either._

import io.circe._
import io.circe.syntax._

package object circe {

  implicit object CirceBuilder extends Builder[Json] {
    def makeFalse: Json = Json.False
    def makeTrue: Json = Json.True
    def makeNull: Json = Json.Null
    def makeString(s: String): Json = Json.fromString(s)
    def makeNumber(s: String): Json = Json.fromJsonNumber(JsonNumber.fromDecimalStringUnsafe(s))
    def makeObject(fields: Iterable[(String, Json)]): Json = Json.fromFields(fields)
    def makeArray(values: Iterable[Json]): Json = Json.fromValues(values)
  }

  implicit object CirceTokenizer extends Tokenizer[Json] {
    import NonEmptyList.one
    private def tokenizeArray(values: Vector[Json]) =
      NonEmptyList(Token.StartArray, values.toList.flatMap(tokenize(_).toList)).append(Token.EndArray)
    private def tokenizeObject(fields: List[(String, Json)]) =
      NonEmptyList(Token.StartObject,
                   fields.flatMap { case (k, v) =>
                     Token.Key(k) :: tokenize(v).toList
                   }).append(Token.EndObject)
    def tokenize(json: Json): NonEmptyList[Token] =
      json.fold(
        one(Token.NullValue),
        b => one(if (b) Token.TrueValue else Token.FalseValue),
        n => one(Token.NumberValue(n.asJson.noSpaces)),
        s => one(Token.StringValue(s)),
        a => tokenizeArray(a),
        o => tokenizeObject(o.toList)
      )
  }

  implicit def deserializerForDecoder[A](implicit decoder: Decoder[A]): Deserializer.Aux[A, Json] =
    new Deserializer[A] {
      type Json = io.circe.Json
      implicit val builder: Builder[Json] = CirceBuilder
      def deserialize(json: Json): Either[JsonException, A] = decoder
        .decodeJson(json)
        .leftMap(t => JsonException("an error occured while deserializing Json values", inner = t))
    }

  implicit def serializerForEncoder[A](implicit encoder: Encoder[A]): Serializer.Aux[A, Json] =
    new Serializer[A] {
      type Json = io.circe.Json
      implicit val tokenizer: Tokenizer[Json] = CirceTokenizer
      def serialize(a: A): Json = encoder(a)
    }

  implicit def tokenizerForEncoder[T](implicit encoder: Encoder[T]): Tokenizer[T] =
    new Tokenizer[T] {
      def tokenize(json: T): NonEmptyList[Token] =
        CirceTokenizer.tokenize(json.asJson)
    }

}
