/*
 * Copyright 2019-2022 Lucas Satabin
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

sealed abstract class Token(val kind: String) {
  def jsonRepr: String
}
object Token {

  case object StartObject extends Token("object") {
    def jsonRepr: String = "{"
  }
  case object EndObject extends Token("<none>") {
    def jsonRepr: String = "}"
  }

  case object StartArray extends Token("array") {
    def jsonRepr: String = "["
  }
  case object EndArray extends Token("<none>") {
    def jsonRepr: String = "]"
  }

  case class Key(value: String) extends Token("key") {
    def jsonRepr: String = s""""$value""""
  }

  case object NullValue extends Token("null") {
    def jsonRepr: String = "null"
  }

  case object TrueValue extends Token("boolean") {
    def jsonRepr: String = "true"
  }
  case object FalseValue extends Token("boolean") {
    def jsonRepr: String = "false"
  }
  case class NumberValue(value: String) extends Token("number") {
    def jsonRepr: String = value
  }
  case class StringValue(value: String) extends Token("string") {
    def jsonRepr: String = s""""$value""""
  }

}
