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

private[json] object TokenSelector {

  private def transformValue[F[_], Json, A, B](chunk: Chunk[Token],
                                               idx: Int,
                                               rest: Stream[F, Token],
                                               toA: Json => Either[JsonException, A],
                                               f: A => B,
                                               fromB: B => Option[Json],
                                               context: JsonContext,
                                               chunkAcc: VectorBuilder[Token],
                                               key: Option[String])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pull[F, Token, Result[F, VectorBuilder[Token]]] =
    ValueParser.pullValue(chunk, idx, rest).flatMap {
      case Some((chunk, idx, rest, json)) =>
        Pull
          .pure(toA(json))
          .flatMap {
            case Left(e) =>
              Pull.raiseError(new JsonException("An error occurred while transforming Json data", Some(context), e))
            case Right(v) =>
              val chunkAcc1 = fromB(f(v)) match {
                case Some(json) => chunkAcc ++= key.map(Token.Key(_)) ++= tokenizer.tokenize(json).toList
                case None       => chunkAcc
              }
              Pull.pure(Some((chunk, idx, rest, chunkAcc1)))
          }
      case None => Pull.pure(None)
    }

  private def transformValueF[F[_], Json, A, B](chunk: Chunk[Token],
                                                idx: Int,
                                                rest: Stream[F, Token],
                                                toA: Json => Either[JsonException, A],
                                                f: A => F[B],
                                                fromB: B => Option[Json],
                                                context: JsonContext,
                                                chunkAcc: VectorBuilder[Token],
                                                key: Option[String])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pull[F, Token, Result[F, VectorBuilder[Token]]] =
    ValueParser.pullValue(chunk, idx, rest).flatMap {
      case Some((chunk, idx, rest, json)) =>
        Pull
          .pure(toA(json))
          .flatMap {
            case Left(e)  => Pull.raiseError(e)
            case Right(v) => Pull.pure(v)
          }
          .flatMap(a => Pull.eval(f(a)))
          .handleErrorWith(t =>
            emitChunk(chunkAcc) >> Pull.raiseError(
              new JsonException("An error occurred while transforming Json data", Some(context), t)))
          .map { transformed =>
            val chunkAcc1 = fromB(transformed) match {
              case Some(transformed) =>
                chunkAcc ++= key.map(Token.Key(_)) ++= tokenizer.tokenize(transformed).toList
              case None => chunkAcc
            }
            Some((chunk, idx, rest, chunkAcc1))
          }
      case None => Pull.pure(None)
    }

  private def selectName[F[_]](chunk: Chunk[Token],
                               idx: Int,
                               rest: Stream[F, Token],
                               emitNonSelected: Boolean,
                               wrap: Boolean,
                               emitEarly: Boolean,
                               toSelect: String => Boolean,
                               mandatories: Set[String],
                               context: JsonContext,
                               onSelect: (Chunk[Token],
                                          Int,
                                          Stream[F, Token],
                                          JsonContext,
                                          VectorBuilder[Token],
                                          Option[String]) => Pull[F, Token, Result[F, VectorBuilder[Token]]],
                               chunkAcc: VectorBuilder[Token])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, VectorBuilder[Token]]] =
    if (idx >= chunk.size) {
      emitChunk(chunkAcc) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) =>
          chunkAcc.clear()
          selectName(hd, 0, tl, emitNonSelected, wrap, emitEarly, toSelect, mandatories, context, onSelect, chunkAcc)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input", Some(context)))
      }
    } else
      chunk(idx) match {
        case key @ Token.Key(name) =>
          val action =
            if (toSelect(name)) {
              // name is to be selected, then continue
              val chunkAcc1 = if (wrap && emitEarly) chunkAcc += key else chunkAcc
              onSelect(chunk, idx + 1, rest, JsonContext.Key(name, context), chunkAcc1, Some(name))
            } else if (emitNonSelected) {
              val chunkAcc1 = if (wrap) chunkAcc += key else chunkAcc
              emitValue(chunk, idx + 1, rest, 0, chunkAcc1)
            } else {
              // skip the value and continue
              skipValue(chunk, idx + 1, rest, 0, chunkAcc)
            }
          action.flatMap {
            case Some((chunk, idx, rest, chunkAcc)) =>
              selectName(chunk,
                         idx,
                         rest,
                         emitNonSelected,
                         wrap,
                         emitEarly,
                         toSelect,
                         mandatories - name,
                         JsonContext.Key(name, context),
                         onSelect,
                         chunkAcc)
            case None =>
              Pull.raiseError[F](new JsonException("unexpected end of input", Some(context)))
          }

        case Token.EndObject =>
          // object is done, go up
          // but first check if all mandatory fields have been seen
          if (mandatories.isEmpty) {
            if (wrap) chunkAcc += Token.EndObject else chunkAcc
            Pull.pure(Some((chunk, idx + 1, rest, chunkAcc)))
          } else {
            emitChunk(chunkAcc) >> Pull.raiseError[F](
              new JsonMissingFieldException(s"missing mandatory fields: ${mandatories.mkString(", ")}", mandatories))
          }
        case _ =>
          emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException("malformed json", Some(context)))
      }

  private def selectIndex[F[_]](chunk: Chunk[Token],
                                idx: Int,
                                rest: Stream[F, Token],
                                emitNonSelected: Boolean,
                                wrap: Boolean,
                                arrIdx: Int,
                                toSelect: Int => Boolean,
                                context: JsonContext,
                                onSelect: (Chunk[Token],
                                           Int,
                                           Stream[F, Token],
                                           JsonContext,
                                           VectorBuilder[Token],
                                           Option[String]) => Pull[F, Token, Result[F, VectorBuilder[Token]]],
                                chunkAcc: VectorBuilder[Token])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, VectorBuilder[Token]]] =
    if (idx >= chunk.size) {
      emitChunk(chunkAcc) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) =>
          chunkAcc.clear()
          selectIndex(hd, 0, tl, emitNonSelected, wrap, arrIdx, toSelect, context, onSelect, chunkAcc)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input", Some(context)))
      }
    } else
      chunk(idx) match {
        case Token.EndArray =>
          // array is done, go up
          if (wrap) chunkAcc += Token.EndArray else chunkAcc
          Pull.pure(Some((chunk, idx + 1, rest, chunkAcc)))
        case _ =>
          val action =
            if (toSelect(arrIdx))
              // index is to be selected, then continue
              onSelect(chunk, idx, rest, JsonContext.Index(arrIdx, context), chunkAcc, None)
            else if (emitNonSelected)
              emitValue(chunk, idx, rest, 0, chunkAcc)
            else
              // skip the value and continue
              skipValue(chunk, idx, rest, 0, chunkAcc)
          action.flatMap {
            case Some((chunk, idx, rest, chunkAcc)) =>
              selectIndex(chunk, idx, rest, emitNonSelected, wrap, arrIdx + 1, toSelect, context, onSelect, chunkAcc)
            case None =>
              Pull.raiseError[F](new JsonException("unexpected end of input", Some(context)))
          }
      }

  private def filterChunk[F[_]](chunk: Chunk[Token],
                                idx: Int,
                                rest: Stream[F, Token],
                                selector: Selector,
                                emitNonSelected: Boolean,
                                wrap: Boolean,
                                emitEarly: Boolean,
                                context: JsonContext,
                                onSelect: (Chunk[Token],
                                           Int,
                                           Stream[F, Token],
                                           JsonContext,
                                           VectorBuilder[Token],
                                           Option[String]) => Pull[F, Token, Result[F, VectorBuilder[Token]]],
                                chunkAcc: VectorBuilder[Token],
                                key: Option[String])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, VectorBuilder[Token]]] =
    if (idx >= chunk.size) {
      emitChunk(chunkAcc) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) =>
          chunkAcc.clear()
          filterChunk(hd, 0, tl, selector, emitNonSelected, wrap, emitEarly, context, onSelect, chunkAcc, key)
        case None => Pull.pure(None)
      }
    } else {
      selector match {
        case Selector.ThisSelector =>
          onSelect(chunk, idx, rest, context, chunkAcc, key)
        case Selector.NameSelector(pred, strict, mandatory) =>
          chunk(idx) match {
            case Token.StartObject =>
              // enter the object context and go down to the name
              if (wrap) chunkAcc += Token.StartObject else chunkAcc
              selectName(chunk,
                         idx + 1,
                         rest,
                         emitNonSelected,
                         wrap,
                         emitEarly,
                         pred,
                         if (mandatory) pred.values else Set.empty,
                         context,
                         onSelect,
                         chunkAcc)
            case token =>
              if (strict)
                emitChunk(chunkAcc) >> Pull.raiseError[F](
                  new JsonException(s"cannot index ${token.kind} with string", Some(context)))
              else if (emitNonSelected)
                emitValue(chunk, idx, rest, 0, chunkAcc)
              else
                // skip the value and go up
                skipValue(chunk, idx, rest, 0, chunkAcc)
          }
        case Selector.IndexSelector(pred, strict) =>
          chunk(idx) match {
            case Token.StartArray =>
              // enter the array context and go down to the indices
              if (wrap) chunkAcc += Token.StartArray else chunkAcc
              selectIndex(chunk, idx + 1, rest, emitNonSelected, wrap, 0, pred, context, onSelect, chunkAcc)
            case token =>
              if (strict)
                emitChunk(chunkAcc) >> Pull.raiseError[F](
                  new JsonException(s"cannot index ${token.kind} with number", Some(context)))
              else if (emitNonSelected)
                emitValue(chunk, idx, rest, 0, chunkAcc)
              else
                // skip the value and go up
                skipValue(chunk, idx, rest, 0, chunkAcc)
          }
        case Selector.IteratorSelector(strict) =>
          chunk(idx) match {
            case Token.StartArray =>
              // enter the array context and go down to the indices
              if (wrap) chunkAcc += Token.StartArray else chunkAcc
              selectIndex(chunk,
                          idx + 1,
                          rest,
                          emitNonSelected,
                          wrap,
                          0,
                          IndexPredicate.All,
                          context,
                          onSelect,
                          chunkAcc)
            case Token.StartObject =>
              // enter the object context and go down to the name
              if (wrap) chunkAcc += Token.StartObject else chunkAcc
              selectName(chunk,
                         idx + 1,
                         rest,
                         emitNonSelected,
                         wrap,
                         emitEarly,
                         NamePredicate.All,
                         Set.empty,
                         context,
                         onSelect,
                         chunkAcc)
            case token =>
              if (strict)
                emitChunk(chunkAcc) >> Pull.raiseError[F](
                  new JsonException(s"cannot iterate over ${token.kind}", Some(context)))
              else if (emitNonSelected)
                emitValue(chunk, idx, rest, 0, chunkAcc)
              else
                // skip the value and go up
                skipValue(chunk, idx, rest, 0, chunkAcc)
          }
        case Selector.PipeSelector(left, right) =>
          filterChunk(
            chunk,
            idx,
            rest,
            left,
            emitNonSelected,
            wrap,
            true,
            context,
            filterChunk(_, _, _, right, emitNonSelected, wrap, emitEarly, _, onSelect, _, _),
            chunkAcc,
            key
          )
      }
    }

  private def go[F[_]](chunk: Chunk[Token],
                       idx: Int,
                       rest: Stream[F, Token],
                       selector: Selector,
                       emitNonSelected: Boolean,
                       wrap: Boolean,
                       emitEarly: Boolean,
                       context: JsonContext,
                       onSelect: (Chunk[Token],
                                  Int,
                                  Stream[F, Token],
                                  JsonContext,
                                  VectorBuilder[Token],
                                  Option[String]) => Pull[F, Token, Result[F, VectorBuilder[Token]]],
                       chunkAcc: VectorBuilder[Token])(implicit F: RaiseThrowable[F]): Pull[F, Token, Unit] =
    filterChunk(chunk, idx, rest, selector, emitNonSelected, wrap, emitEarly, context, onSelect, chunkAcc, None)
      .flatMap {
        case Some((chunk, idx, rest, chunkAcc)) =>
          go(chunk, idx, rest, selector, emitNonSelected, wrap, emitEarly, context, onSelect, chunkAcc)
        case None =>
          Pull.done
      }

  def pipe[F[_]](selector: Selector, wrap: Boolean)(implicit F: RaiseThrowable[F]): Pipe[F, Token, Token] =
    s => go(Chunk.empty, 0, s, selector, false, wrap, true, JsonContext.Root, emit[F], new VectorBuilder).stream

  def transformPipe[F[_], Json: Builder: Tokenizer, A, B](selector: Selector,
                                                          toA: Json => Either[JsonException, A],
                                                          f: A => B,
                                                          fromB: B => Option[Json],
                                                          emitEarly: Boolean)(implicit
      F: RaiseThrowable[F]): Pipe[F, Token, Token] =
    s =>
      go(Chunk.empty,
         0,
         s,
         selector,
         true,
         true,
         emitEarly,
         JsonContext.Root,
         transform[F, Json, A, B](toA, f, fromB),
         new VectorBuilder).stream

  private def emit[F[_]](chunk: Chunk[Token],
                         idx: Int,
                         rest: Stream[F, Token],
                         context: JsonContext,
                         chunkAcc: VectorBuilder[Token],
                         key: Option[String])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, VectorBuilder[Token]]] =
    emitValue[F](chunk, idx, rest, 0, chunkAcc)

  def transformPipeF[F[_], Json: Builder: Tokenizer, A, B](selector: Selector,
                                                           toA: Json => Either[JsonException, A],
                                                           f: A => F[B],
                                                           fromB: B => Option[Json],
                                                           emitEarly: Boolean)(implicit
      F: RaiseThrowable[F]): Pipe[F, Token, Token] =
    s =>
      go(Chunk.empty,
         0,
         s,
         selector,
         true,
         true,
         emitEarly,
         JsonContext.Root,
         transformF(toA, f, fromB),
         new VectorBuilder).stream

  private def transform[F[_], Json: Builder: Tokenizer, A, B](toA: Json => Either[JsonException, A],
                                                              f: A => B,
                                                              fromB: B => Option[Json])(chunk: Chunk[Token],
                                                                                        idx: Int,
                                                                                        rest: Stream[F, Token],
                                                                                        context: JsonContext,
                                                                                        chunkAcc: VectorBuilder[Token],
                                                                                        key: Option[String])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, VectorBuilder[Token]]] =
    transformValue(chunk, idx, rest, toA, f, fromB, context, chunkAcc, key)

  private def transformF[F[_], Json: Builder: Tokenizer, A, B](toA: Json => Either[JsonException, A],
                                                               f: A => F[B],
                                                               fromB: B => Option[Json])(chunk: Chunk[Token],
                                                                                         idx: Int,
                                                                                         rest: Stream[F, Token],
                                                                                         context: JsonContext,
                                                                                         chunkAcc: VectorBuilder[Token],
                                                                                         key: Option[String])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, VectorBuilder[Token]]] =
    transformValueF(chunk, idx, rest, toA, f, fromB, context, chunkAcc, key)

}
