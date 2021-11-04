/*
 * Copyright 2021 Lucas Satabin
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
package codec

import ast.Builder

/** Tells how some Json AST is deserialized into some value.
  */
trait Deserializer[A] {
  type Json
  implicit val builder: Builder[Json]
  def deserialize(json: Json): Either[JsonException, A]
}

object Deserializer {
  type Aux[A, J] = Deserializer[A] { type Json = J }
}