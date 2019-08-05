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

sealed abstract class Token(val kind: String)
object Token {

  case object StartObject extends Token("object")
  case object EndObject extends Token("<none>")

  case object StartArray extends Token("array")
  case object EndArray extends Token("<none>")

  case class Key(value: String) extends Token("key")

  case object NullValue extends Token("null")

  case object TrueValue extends Token("boolean")
  case object FalseValue extends Token("boolean")
  case class NumberValue(value: String) extends Token("number")
  case class StringValue(value: String) extends Token("string")

}
