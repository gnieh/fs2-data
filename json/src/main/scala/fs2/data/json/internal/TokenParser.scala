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

import text._

import cats.syntax.either._

private[json] object TokenParser {

  private[json] final val keyBufferCapacity =
    Option(System.getProperty("fs2.data.json.key-buffer-capacity"))
      .flatMap(s => Either.catchNonFatal(s.toInt).toOption)
      .getOrElse(64)
  private[json] final val numberBufferCapacity =
    Option(System.getProperty("fs2.data.json.number-buffer-capacity"))
      .flatMap(s => Either.catchNonFatal(s.toInt).toOption)
      .getOrElse(16)
  private[json] final val stringBufferCapacity =
    Option(System.getProperty("fs2.data.json.string-buffer-capacity"))
      .flatMap(s => Either.catchNonFatal(s.toInt).toOption)
      .getOrElse(128)

  def pipe[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): Pipe[F, T, Token] = { s =>
    T match {
      case asCharBuffer: AsCharBuffer[F, T] =>
        Stream.suspend(
          new JsonTokenParser[F, T, Token](s, new TokenChunkAccumulator)(F, asCharBuffer).go_(State.BeforeValue).stream)
      case _ =>
        Stream.suspend(new LegacyTokenParser[F, T, Token](s).parse(new TokenChunkAccumulator).stream)
    }

  }
}
