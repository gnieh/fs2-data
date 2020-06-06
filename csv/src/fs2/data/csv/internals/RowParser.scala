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
package csv
package internals

import text._

import cats.data.{State => _, _}

private[csv] object RowParser {

  def pipe[F[_], T](separator: Char)(implicit F: RaiseThrowable[F],
                                     T: CharLikeChunks[F, T]): Pipe[F, T, NonEmptyList[String]] = {

    def rows(context: T.Context,
             currentField: StringBuilder,
             tail: List[String],
             state: State,
             chunkAcc: List[NonEmptyList[String]]): Pull[F, NonEmptyList[String], Unit] =
      if (T.needsPull(context)) {
        Pull.output(Chunk.seq(chunkAcc.reverse)) >> T.pullNext(context).flatMap {
          case Some(context) => rows(context, currentField, tail, state, Nil)
          case None          =>
            // stream is exhausted, emit potential last line
            state match {
              case State.BeginningOfField =>
                if (tail.nonEmpty)
                  Pull.output1(NonEmptyList("", tail).reverse) >> Pull.done
                else
                  Pull.done
              case State.InUnquoted | State.InQuotedSeenQuote | State.ExpectNewLine =>
                Pull.output1(NonEmptyList(currentField.result, tail).reverse) >> Pull.done
              case State.InUnquotedSeenCr =>
                Pull.output1(NonEmptyList(currentField.append('\r').result, tail).reverse) >> Pull.done
              case State.InQuoted =>
                Pull.raiseError[F](new CsvException("unexpected end of input"))
            }
        }
      } else {
        val c = T.current(context)
        state match {
          case State.InQuoted =>
            // only handle quote specially
            if (c == '"') {
              rows(T.advance(context), currentField, tail, State.InQuotedSeenQuote, chunkAcc)
            } else {
              rows(T.advance(context), currentField.append(c), tail, State.InQuoted, chunkAcc)
            }
          case State.InQuotedSeenQuote =>
            if (c == '"') {
              rows(T.advance(context), currentField.append(c), tail, State.InQuoted, chunkAcc)
            } else if (c == separator) {
              // end of quoted field, go to next
              val field = currentField.result
              currentField.clear
              rows(T.advance(context), currentField, field :: tail, State.BeginningOfField, chunkAcc)
            } else if (c == '\n') {
              val field = currentField.result
              currentField.clear
              rows(T.advance(context),
                   currentField,
                   Nil,
                   State.BeginningOfField,
                   NonEmptyList(field, tail).reverse :: chunkAcc)
            } else if (c == '\r') {
              rows(T.advance(context), currentField, tail, State.ExpectNewLine, chunkAcc)
            } else {
              // this is an error
              Pull.raiseError[F](new CsvException(s"unexpected character '$c'"))
            }
          case State.ExpectNewLine =>
            if (c == '\n') {
              val field = currentField.result
              currentField.clear
              rows(T.advance(context),
                   currentField,
                   Nil,
                   State.BeginningOfField,
                   NonEmptyList(field, tail).reverse :: chunkAcc)
            } else {
              // this is an error
              Pull.raiseError[F](new CsvException(s"unexpected character '$c'"))
            }
          case State.BeginningOfField =>
            if (c == '"') {
              // start a quoted field
              rows(T.advance(context), currentField, tail, State.InQuoted, chunkAcc)
            } else if (c == separator) {
              // this is an empty field
              rows(T.advance(context), currentField, "" :: tail, State.BeginningOfField, chunkAcc)
            } else if (c == '\n') {
              // a new line, emit row if not empty and continue
              if (tail.nonEmpty) {
                rows(T.advance(context),
                     currentField,
                     Nil,
                     State.BeginningOfField,
                     NonEmptyList("", tail).reverse :: chunkAcc)
              } else {
                rows(T.advance(context), currentField, Nil, State.BeginningOfField, chunkAcc)
              }
            } else if (c == '\r') {
              rows(T.advance(context), currentField, tail, State.InUnquotedSeenCr, chunkAcc)
            } else {
              rows(T.advance(context), currentField.append(c), tail, State.InUnquoted, chunkAcc)
            }
          case State.InUnquoted =>
            if (c == separator) {
              // this is the end of the field, not the row
              val field = currentField.result
              currentField.clear
              rows(T.advance(context), currentField, field :: tail, State.BeginningOfField, chunkAcc)
            } else if (c == '\n') {
              // a new line, emit row and continue
              val field = currentField.result
              currentField.clear
              rows(T.advance(context),
                   currentField,
                   Nil,
                   State.BeginningOfField,
                   NonEmptyList(field, tail).reverse :: chunkAcc)
            } else if (c == '\r') {
              rows(T.advance(context), currentField, tail, State.InUnquotedSeenCr, chunkAcc)
            } else {
              rows(T.advance(context), currentField.append(c), tail, State.InUnquoted, chunkAcc)
            }
          case State.InUnquotedSeenCr =>
            if (c == '\n') {
              // a new line, emit row if not empty and continue
              val field = currentField.result
              currentField.clear
              rows(T.advance(context),
                   currentField,
                   Nil,
                   State.BeginningOfField,
                   NonEmptyList(field, tail).reverse :: chunkAcc)
            } else {
              currentField.append('\r')
              if (c == separator) {
                // this is the end of the field, not the row
                val field = currentField.result
                currentField.clear
                rows(T.advance(context), currentField, field :: tail, State.BeginningOfField, chunkAcc)
              } else {
                // continue parsing field
                currentField.append(c)
                rows(T.advance(context), currentField, tail, State.InUnquoted, chunkAcc)
              }
            }
        }
      }

    s =>
      Stream
        .suspend(Stream.emit(T.create(s)))
        .flatMap(rows(_, new StringBuilder, Nil, State.BeginningOfField, Nil).stream)
  }

}
