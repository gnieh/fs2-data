/*
 * Copyright 2019 Lucas Satabin
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
package fs2
package data
package json

import ast._

import cats.data.NonEmptyList

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
    private def one(token: Token) = NonEmptyList.one(token)
    private def tokenizeArray(values: Vector[Json]) =
      NonEmptyList(Token.StartArray, values.toList.flatMap(tokenize(_).toList)).append(Token.EndArray)
    private def tokenizeObject(fields: List[(String, Json)]) =
      NonEmptyList(Token.StartObject, fields.flatMap {
        case (k, v) => Token.Key(k) :: tokenize(v).toList
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

}
