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

import ast.Builder

import io.circe._

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

}
