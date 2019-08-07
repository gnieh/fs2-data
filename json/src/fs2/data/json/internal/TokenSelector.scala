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
package internals

import cats._

import scala.annotation.switch

import scala.language.higherKinds

private[json] object TokenSelector {

  private def emitValue[F[_]](chunk: Chunk[Token], idx: Int, rest: Stream[F, Token], depth: Int, chunkAcc: List[Token])(
      implicit F: ApplicativeError[F, Throwable]): Pull[F, Token, Result[F, Token, List[Token]]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >>
        rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            emitValue(hd, 0, tl, depth, Nil)
          case None =>
            Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
    } else
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

  private def skipValue[F[_]](chunk: Chunk[Token], idx: Int, rest: Stream[F, Token], depth: Int, chunkAcc: List[Token])(
      implicit F: ApplicativeError[F, Throwable]): Pull[F, Token, Result[F, Token, List[Token]]] =
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

  private def selectName[F[_]](
      chunk: Chunk[Token],
      idx: Int,
      rest: Stream[F, Token],
      wrap: Boolean,
      toSelect: String => Boolean,
      onSelect: (Chunk[Token], Int, Stream[F, Token], List[Token]) => Pull[F, Token, Result[F, Token, List[Token]]],
      chunkAcc: List[Token])(implicit F: ApplicativeError[F, Throwable]): Pull[F, Token, Result[F, Token, List[Token]]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >>
        rest.pull.uncons.flatMap {
          case Some((hd, tl)) => selectName(hd, 0, tl, wrap, toSelect, onSelect, Nil)
          case None           => Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
    } else
      chunk(idx) match {
        case key @ Token.Key(name) if toSelect(name) =>
          // name is to be selected, then continue
          val chunkAcc1 = if (wrap) key :: chunkAcc else chunkAcc
          onSelect(chunk, idx + 1, rest, chunkAcc1).flatMap {
            case Some((chunk, idx, rest, chunkAcc)) =>
              selectName(chunk, idx, rest, wrap, toSelect, onSelect, chunkAcc)
            case None =>
              Pull.raiseError[F](new JsonException("unexpected end of input"))
          }
        case Token.Key(_) =>
          // skip the value and continue
          skipValue(chunk, idx + 1, rest, 0, chunkAcc).flatMap {
            case Some((chunk, idx, rest, chunkAcc)) =>
              selectName(chunk, idx, rest, wrap, toSelect, onSelect, chunkAcc)
            case None =>
              Pull.raiseError[F](new JsonException("unexpected end of input"))
          }
        case Token.EndObject =>
          // object is done, go up
          val chunkAcc1 = if (wrap) Token.EndObject :: chunkAcc else chunkAcc
          Pull.pure(Some((chunk, idx + 1, rest, chunkAcc1)))
      }

  private def selectIndex[F[_]](
      chunk: Chunk[Token],
      idx: Int,
      rest: Stream[F, Token],
      wrap: Boolean,
      arrIdx: Int,
      toSelect: Int => Boolean,
      onSelect: (Chunk[Token], Int, Stream[F, Token], List[Token]) => Pull[F, Token, Result[F, Token, List[Token]]],
      chunkAcc: List[Token])(implicit F: ApplicativeError[F, Throwable]): Pull[F, Token, Result[F, Token, List[Token]]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >>
        rest.pull.uncons.flatMap {
          case Some((hd, tl)) => selectIndex(hd, 0, tl, wrap, arrIdx, toSelect, onSelect, Nil)
          case None           => Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
    } else
      chunk(idx) match {
        case Token.EndArray =>
          // array is done, go up
          val chunkAcc1 = if (wrap) Token.EndArray :: chunkAcc else chunkAcc
          Pull.pure(Some((chunk, idx + 1, rest, chunkAcc1)))
        case token =>
          if (toSelect(arrIdx)) {
            // index is to be selected, then continue
            onSelect(chunk, idx, rest, chunkAcc).flatMap {
              case Some((chunk, idx, rest, chunkAcc)) =>
                selectIndex(chunk, idx, rest, wrap, arrIdx + 1, toSelect, onSelect, chunkAcc)
              case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
            }
          } else {
            // skip the value and continue
            skipValue(chunk, idx, rest, 0, chunkAcc).flatMap {
              case Some((chunk, idx, rest, chunkAcc)) =>
                selectIndex(chunk, idx, rest, wrap, arrIdx + 1, toSelect, onSelect, chunkAcc)
              case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
            }
          }
      }

  private def filterChunk[F[_]](
      chunk: Chunk[Token],
      idx: Int,
      rest: Stream[F, Token],
      selector: Selector,
      wrap: Boolean,
      onSelect: (Chunk[Token], Int, Stream[F, Token], List[Token]) => Pull[F, Token, Result[F, Token, List[Token]]],
      chunkAcc: List[Token])(implicit F: ApplicativeError[F, Throwable]): Pull[F, Token, Result[F, Token, List[Token]]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >>
        rest.pull.uncons.flatMap {
          case Some((hd, tl)) => filterChunk(hd, 0, tl, selector, wrap, onSelect, Nil)
          case None           => Pull.pure(None)
        }
    } else
      selector match {
        case Selector.ThisSelector =>
          onSelect(chunk, idx, rest, chunkAcc)
        case Selector.NameSelector(pred, strict) =>
          chunk(idx) match {
            case Token.StartObject =>
              // enter the object context and go down to the name
              val chunkAcc1 = if (wrap) Token.StartObject :: chunkAcc else chunkAcc
              selectName(chunk, idx + 1, rest, wrap, pred, onSelect, chunkAcc1)
            case token =>
              if (strict)
                Pull.raiseError[F](new JsonException(s"cannot ${token.kind} number with string"))
              else
                // skip the value and go up
                skipValue(chunk, idx, rest, 0, chunkAcc)
          }
        case Selector.IndexSelector(pred, strict) =>
          chunk(idx) match {
            case Token.StartArray =>
              // enter the array context and go down to the indices
              val chunkAcc1 = if (wrap) Token.StartArray :: chunkAcc else chunkAcc
              selectIndex(chunk, idx + 1, rest, wrap, 0, pred, onSelect, chunkAcc1)
            case token =>
              if (strict)
                Pull.raiseError[F](new JsonException(s"cannot index ${token.kind} with number"))
              else
                // skip the value and go up
                skipValue(chunk, idx, rest, 0, chunkAcc)
          }
        case Selector.IteratorSelector(strict) =>
          chunk(idx) match {
            case Token.StartArray =>
              // enter the array context and go down to the indices
              val chunkAcc1 = if (wrap) Token.StartArray :: chunkAcc else chunkAcc
              selectIndex(chunk, idx + 1, rest, wrap, 0, IndexPredicate.All, onSelect, chunkAcc1)
            case Token.StartObject =>
              // enter the object context and go down to the name
              val chunkAcc1 = if (wrap) Token.StartObject :: chunkAcc else chunkAcc
              selectName(chunk, idx + 1, rest, wrap, NamePredicate.All, onSelect, chunkAcc1)
            case token =>
              if (strict)
                Pull.raiseError[F](new JsonException(s"cannot iterate over ${token.kind}"))
              else
                // skip the value and go up
                skipValue(chunk, idx, rest, 0, chunkAcc)
          }
        case Selector.PipeSelector(left, right) =>
          filterChunk(chunk, idx, rest, left, wrap, filterChunk(_, _, _, right, wrap, onSelect, _), chunkAcc)
      }

  private def go[F[_]](chunk: Chunk[Token],
                       idx: Int,
                       rest: Stream[F, Token],
                       selector: Selector,
                       wrap: Boolean,
                       onSelect: (Chunk[Token], Int, Stream[F, Token], List[Token]) => Pull[F, Token, Result[F, Token, List[Token]]],
                       chunkAcc: List[Token])(implicit F: ApplicativeError[F, Throwable]): Pull[F, Token, Unit] =
    filterChunk(chunk, idx, rest, selector, wrap: Boolean, onSelect, chunkAcc).flatMap {
      case Some((chunk, idx, rest, chunkAcc)) =>
        go(chunk, idx, rest, selector, wrap, onSelect, chunkAcc)
      case None =>
        Pull.done
    }

  def pipe[F[_]](selector: Selector, wrap: Boolean)(implicit F: ApplicativeError[F, Throwable]): Pipe[F, Token, Token] =
    s => go(Chunk.empty, 0, s, selector, wrap, emit[F](0), Nil).stream

  private def emit[F[_]](depth: Int)(chunk: Chunk[Token], idx: Int, rest: Stream[F, Token], chunkAcc: List[Token])(
      implicit F: ApplicativeError[F, Throwable]): Pull[F, Token, Result[F, Token, List[Token]]] =
    emitValue[F](chunk, idx, rest, 0, chunkAcc)

}

private sealed trait Context
private object Context {
  case object Root extends Context
  case class InArray(at: Int) extends Context
  case object InObject extends Context
}
