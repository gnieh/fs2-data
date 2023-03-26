/*
 * Copyright 2023 Lucas Satabin
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

import _root_.play.api.libs.json._
import cats.data.NonEmptyList
import cats.syntax.either._

package object playJson {

  implicit object PlayBuilder extends Builder[JsValue] {
    def makeTrue: JsValue = JsTrue
    def makeFalse: JsValue = JsFalse
    def makeNull: JsValue = JsNull
    def makeString(s: String): JsValue = JsString(s)
    def makeNumber(s: String): JsValue = JsNumber(BigDecimal(s))
    def makeObject(fields: Iterable[(String, JsValue)]): JsValue = JsObject(fields.toSeq)
    def makeArray(values: Iterable[JsValue]): JsValue = JsArray(values.toSeq)
  }

  implicit object PlayTokenizer extends Tokenizer[JsValue] {
    import NonEmptyList.one
    def tokenize(json: JsValue): NonEmptyList[Token] = json match {
      case JsNull          => one(Token.NullValue)
      case JsTrue          => one(Token.TrueValue)
      case JsFalse         => one(Token.FalseValue)
      case JsNumber(value) => one(Token.NumberValue(value.toString))
      case JsString(value) => one(Token.StringValue(value))
      case JsArray(values) =>
        NonEmptyList(Token.StartArray, values.toList.flatMap(tokenize(_).toList)).append(Token.EndArray)
      case JsObject(fields) =>
        NonEmptyList(Token.StartObject,
                     fields.toList
                       .flatMap { case (k, v) =>
                         Token.Key(k) :: tokenize(v).toList
                       })
          .append(Token.EndObject)
    }
  }

  implicit def deserializerForReads[A](implicit reads: Reads[A]): Deserializer.Aux[A, JsValue] =
    new Deserializer[A] {
      type Json = JsValue
      implicit val builder: Builder[JsValue] = PlayBuilder
      def deserialize(json: JsValue): Either[JsonException, A] =
        Either
          .catchNonFatal(json.as[A])
          .leftMap(t => JsonException("an error occured while deserializing Json values", inner = t))
    }

  implicit def serializerForWrites[A](implicit writes: Writes[A]): Serializer.Aux[A, JsValue] =
    new Serializer[A] {
      type Json = JsValue
      implicit val tokenizer: Tokenizer[JsValue] = PlayTokenizer
      def serialize(a: A): JsValue = Json.toJson(a)
    }

  implicit def tokenizerForWrites[T](implicit writes: Writes[T]): Tokenizer[T] =
    new Tokenizer[T] {
      def tokenize(json: T): NonEmptyList[Token] =
        PlayTokenizer.tokenize(Json.toJson(json))
    }

}
