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

package fs2
package data
package json
package internals

import ast.Tokenizer

private[json] object ObjectWrapper {

  def pipe[F[_], Json](at: String, in: Map[String, Json], mapFirst: Boolean, single: Boolean)(implicit
      tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] = { s =>
    val mapStream = Stream.emits(in.flatMap { case (key, value) =>
      Token.Key(key) :: tokenizer.tokenize(value).toList
    }.toSeq)
    val wrappedStream = if (single) s else Stream.emit(Token.StartArray) ++ s ++ Stream.emit(Token.EndArray)
    val kvStream =
      if (mapFirst) {
        mapStream ++ Stream.emit(Token.Key(at)) ++ wrappedStream
      } else {
        Stream.emit(Token.Key(at)) ++ wrappedStream ++ mapStream
      }
    Stream.emit(Token.StartObject) ++ kvStream ++ Stream.emit(Token.EndObject)
  }

}
