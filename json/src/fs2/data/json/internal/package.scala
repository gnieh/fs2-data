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

import scala.collection.immutable.VectorBuilder

package object internals {

  private[internals] val hexa = "0123456789abcdef"

  private[json] type Result[F[_], Out] = Option[(Chunk[Token], Int, Stream[F, Token], Out)]

  private[json] def emitChunk[T](chunkAcc: VectorBuilder[T]) =
    Pull.output(Chunk.vector(chunkAcc.result()))

  private[json] def skipValue[F[_]](chunk: Chunk[Token],
                                    idx: Int,
                                    rest: Stream[F, Token],
                                    depth: Int,
                                    chunkAcc: List[Token])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, List[Token]]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >>
        rest.pull.uncons.flatMap {
          case Some((hd, tl)) => skipValue(hd, 0, tl, depth, Nil)
          case None           => Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
    } else
      chunk(idx) match {
        case Token.StartArray | Token.StartObject =>
          skipValue(chunk, idx + 1, rest, depth + 1, chunkAcc)
        case Token.EndArray | Token.EndObject =>
          if (depth == 1)
            // this is the value closing token, skip it and we are done
            Pull.pure(Some((chunk, idx + 1, rest, chunkAcc)))
          else if (depth < 1)
            // this is an error, closing a not opened brace or bracket
            Pull.raiseError[F](new JsonException("malformed json"))
          else
            skipValue(chunk, idx + 1, rest, depth - 1, chunkAcc)
        case _ =>
          if (depth == 0)
            // this is the value to skip
            Pull.pure(Some((chunk, idx + 1, rest, chunkAcc)))
          else
            skipValue(chunk, idx + 1, rest, depth, chunkAcc)
      }

  private[json] def emitValue[F[_]](chunk: Chunk[Token],
                                    idx: Int,
                                    rest: Stream[F, Token],
                                    depth: Int,
                                    chunkAcc: List[Token])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, List[Token]]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >>
        rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            emitValue(hd, 0, tl, depth, Nil)
          case None =>
            Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
    } else {
      chunk(idx) match {
        case token @ (Token.StartArray | Token.StartObject) =>
          emitValue(chunk, idx + 1, rest, depth + 1, token :: chunkAcc)
        case token @ (Token.EndArray | Token.EndObject) =>
          if (depth == 1)
            // this is the value closing token, onSelect it and we are done
            Pull.pure(Some((chunk, idx + 1, rest, token :: chunkAcc)))
          else if (depth < 1)
            // this is an error, closing a not opened brace or bracket
            Pull.raiseError[F](new JsonException("malformed json"))
          else
            emitValue(chunk, idx + 1, rest, depth - 1, token :: chunkAcc)
        case token =>
          if (depth == 0)
            // this is the value to onSelect
            Pull.pure(Some((chunk, idx + 1, rest, token :: chunkAcc)))
          else
            emitValue(chunk, idx + 1, rest, depth, token :: chunkAcc)
      }
    }

}
