/*
 * Copyright 2024 fs2-data Project
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
package ast

import cats.data.NonEmptyList

/** Transforms a parsed json value into tokens.
  * This is intended to be used to generate token stream
  * out of a value.
  */
trait Tokenizer[Json] {

  def tokenize(json: Json): NonEmptyList[Token]

}

object Tokenizer {

  implicit object NELTokenizer extends Tokenizer[NonEmptyList[Token]] {
    def tokenize(json: NonEmptyList[Token]): NonEmptyList[Token] = json
  }

}
