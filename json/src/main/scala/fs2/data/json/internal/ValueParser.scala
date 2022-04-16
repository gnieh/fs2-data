/*
 * Copyright 2022 Lucas Satabin
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

import ast._

import scala.collection.immutable.VectorBuilder

private[json] object ValueParser {

  private def pullArray[F[_], Json](chunk: Chunk[Token], idx: Int, rest: Stream[F, Token], acc: VectorBuilder[Json])(
      implicit
      F: RaiseThrowable[F],
      builder: Builder[Json]): Pull[F, INothing, Result[F, Json]] =
    if (idx >= chunk.size) {
      rest.pull.uncons.flatMap {
        case Some((hd, tl)) => pullArray(hd, 0, tl, acc)
        case None           => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      chunk(idx) match {
        case Token.EndArray =>
          Pull.pure(Some((chunk, idx + 1, rest, builder.makeArray(acc.result()))))
        case _ =>
          Pull.suspend(pullValue(chunk, idx, rest).flatMap {
            case Some((chunk, idx, rest, json)) => pullArray(chunk, idx, rest, acc += json)
            case None                           => Pull.raiseError[F](new JsonException("unexpected end of input"))
          })
      }
    }

  private def pullObject[F[_], Json](chunk: Chunk[Token],
                                     idx: Int,
                                     rest: Stream[F, Token],
                                     acc: VectorBuilder[(String, Json)])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json]): Pull[F, INothing, Result[F, Json]] =
    if (idx >= chunk.size) {
      rest.pull.uncons.flatMap {
        case Some((hd, tl)) => pullObject(hd, 0, tl, acc)
        case None           => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      chunk(idx) match {
        case Token.EndObject =>
          Pull.pure(Some((chunk, idx + 1, rest, builder.makeObject(acc.result()))))
        case Token.Key(key) =>
          pullValue(chunk, idx + 1, rest).flatMap {
            case Some((chunk, idx, rest, json)) => pullObject(chunk, idx, rest, acc += (key -> json))
            case None                           => Pull.raiseError[F](new JsonException("unexpected end of input"))
          }
        case token =>
          Pull.raiseError[F](new JsonException(s"malformed json (unexpected $token)"))
      }
    }

  def pullValue[F[_], Json](chunk: Chunk[Token], idx: Int, rest: Stream[F, Token])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json]): Pull[F, INothing, Result[F, Json]] =
    if (idx >= chunk.size) {
      rest.pull.uncons.flatMap {
        case Some((hd, tl)) => pullValue(hd, 0, tl)
        case None           => Pull.pure(None)
      }
    } else {
      chunk(idx) match {
        case Token.TrueValue      => Pull.pure(Some((chunk, idx + 1, rest, builder.makeTrue)))
        case Token.FalseValue     => Pull.pure(Some((chunk, idx + 1, rest, builder.makeFalse)))
        case Token.NullValue      => Pull.pure(Some((chunk, idx + 1, rest, builder.makeNull)))
        case Token.StringValue(s) => Pull.pure(Some((chunk, idx + 1, rest, builder.makeString(s))))
        case Token.NumberValue(s) => Pull.pure(Some((chunk, idx + 1, rest, builder.makeNumber(s))))
        case Token.StartArray     => pullArray(chunk, idx + 1, rest, new VectorBuilder)
        case Token.StartObject    => pullObject(chunk, idx + 1, rest, new VectorBuilder)
        case token                => Pull.raiseError[F](new JsonException(s"malformed json (unexpected $token)"))
      }
    }

  /** Pulls one json value from the stream if any, builds the AST for it
    * and returns it with the rest stream.
    */
  def pullOne[F[_], Json](s: Stream[F, Token])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json]): Pull[F, INothing, Option[(Json, Stream[F, Token])]] =
    s.pull.uncons.flatMap {
      case Some((hd, tl)) =>
        pullValue(hd, 0, tl).map {
          case Some((chunk, idx, rest, json)) => Some((json, Stream.chunk(chunk.drop(idx)) ++ rest))
          case None                           => None
        }
      case None => Pull.pure(None)
    }

  /** Pulls and emits all json values from the stream if any, builds the AST foreach value.
    */
  def pullAll[F[_], Json](
      s: Stream[F, Token])(implicit F: RaiseThrowable[F], builder: Builder[Json]): Pull[F, Json, Unit] = {
    def go(chunk: Chunk[Token], idx: Int, rest: Stream[F, Token], chunkAcc: List[Json]): Pull[F, Json, Unit] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) => go(hd, 0, tl, Nil)
          case None           => Pull.done
        }
      } else {
        pullValue(chunk, idx, rest)
          .handleErrorWith(t => Pull.output(Chunk.seq(chunkAcc.reverse)) >> Pull.raiseError(t))
          .flatMap {
            case Some((chunk, idx, rest, json)) => go(chunk, idx, rest, json :: chunkAcc)
            case None                           => Pull.done
          }
      }
    go(Chunk.empty, 0, s, Nil)
  }

  def pipe[F[_], Json](implicit F: RaiseThrowable[F], builder: Builder[Json]): Pipe[F, Token, Json] =
    s => pullAll(s).stream

}
