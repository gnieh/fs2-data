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

import cats.data.{State => _, _}

private[csv] object RowParser {

  def pipe[F[_]](separator: Char, quoteHandling: QuoteHandling)(
      implicit F: RaiseThrowable[F]): Pipe[F, Char, (NonEmptyList[String], Long, Long)] = {

    def row(chunk: Chunk[Char],
            currentField: StringBuilder,
            tail: List[String],
            state: State,
            idx: Int,
            start: Long,
            current: Long): Pull[F, (NonEmptyList[String], Long, Long), ParseEnv] =
      if (idx >= chunk.size) {
        Pull.pure(ParseEnv(currentField, tail, state, 0, start, current))
      } else {
        val c = chunk(idx)
        state match {
          case State.InQuoted =>
            // only handle quote specially
            if (c == '"') {
              row(chunk, currentField, tail, State.InQuotedSeenQuote, idx + 1, start, current)
            } else if (c == '\n') {
              row(chunk, currentField.append(c), tail, State.InQuoted, idx + 1, start, current + 1)
            } else {
              row(chunk, currentField.append(c), tail, State.InQuoted, idx + 1, start, current)
            }
          case State.InQuotedSeenQuote =>
            if (c == '"') {
              row(chunk, currentField.append(c), tail, State.InQuoted, idx + 1, start, current)
            } else if (c == separator) {
              // end of quoted field, go to next
              val field = currentField.result
              currentField.clear
              row(chunk, currentField, field :: tail, State.BeginningOfField, idx + 1, start, current)
            } else if (c == '\n') {
              val field = currentField.result
              currentField.clear
              Pull.output1((NonEmptyList(field, tail).reverse, start, current)) >>
                row(chunk, currentField, Nil, State.BeginningOfField, idx + 1, current + 1, current + 1)
            } else if (c == '\r') {
              row(chunk, currentField, tail, State.ExpectNewLine, idx + 1, start, current)
            } else {
              // this is an error
              Pull.raiseError[F](new ParseError(s"unexpected character '$c'", current))
            }
          case State.ExpectNewLine =>
            if (c == '\n') {
              val field = currentField.result
              currentField.clear
              Pull.output1((NonEmptyList(field, tail).reverse, start, current)) >>
                row(chunk, currentField, Nil, State.BeginningOfField, idx + 1, current + 1, current + 1)
            } else {
              // this is an error
              Pull.raiseError[F](new ParseError(s"unexpected character '$c'", current))
            }
          case State.BeginningOfField =>
            if (c == '"' && quoteHandling == QuoteHandling.RFCCompliant) {
              // start a quoted field
              row(chunk, currentField, tail, State.InQuoted, idx + 1, start, current)
            } else if (c == separator) {
              // this is an empty field
              row(chunk, currentField, "" :: tail, State.BeginningOfField, idx + 1, start, current)
            } else if (c == '\n') {
              // a new line, emit row if not empty and continue
              if (tail.nonEmpty) {
                Pull.output1((NonEmptyList("", tail).reverse, start, current)) >>
                  row(chunk, currentField, Nil, State.BeginningOfField, idx + 1, current + 1, current + 1)
              } else {
                row(chunk, currentField, Nil, State.BeginningOfField, idx + 1, start, current)
              }
            } else if (c == '\r') {
              row(chunk, currentField, tail, State.InUnquotedSeenCr, idx + 1, start, current)
            } else {
              row(chunk, currentField.append(c), tail, State.InUnquoted, idx + 1, start, current)
            }
          case State.InUnquoted =>
            if (c == separator) {
              // this is the end of the field, not the row
              val field = currentField.result
              currentField.clear
              row(chunk, currentField, field :: tail, State.BeginningOfField, idx + 1, start, current)
            } else if (c == '\n') {
              // a new line, emit row and continue
              val field = currentField.result
              currentField.clear
              Pull.output1((NonEmptyList(field, tail).reverse, start, current)) >>
                row(chunk, currentField, Nil, State.BeginningOfField, idx + 1, current + 1, current + 1)
            } else if (c == '\r') {
              row(chunk, currentField, tail, State.InUnquotedSeenCr, idx + 1, start, current)
            } else {
              row(chunk, currentField.append(c), tail, State.InUnquoted, idx + 1, start, current)
            }
          case State.InUnquotedSeenCr =>
            if (c == '\n') {
              // a new line, emit row if not empty and continue
              val field = currentField.result
              currentField.clear
              Pull.output1((NonEmptyList(field, tail).reverse, start, current)) >>
                row(chunk, currentField, Nil, State.BeginningOfField, idx + 1, current + 1, current + 1)
            } else {
              currentField.append('\r')
              if (c == separator) {
                // this is the end of the field, not the row
                val field = currentField.result
                currentField.clear
                row(chunk, currentField, field :: tail, State.BeginningOfField, idx + 1, start, current)
              } else {
                // continue parsing field
                currentField.append(c)
                row(chunk, currentField, tail, State.InUnquoted, idx + 1, start, current)
              }
            }
        }
      }

    def go(s: Stream[F, Char], env: ParseEnv): Pull[F, (NonEmptyList[String], Long, Long), Unit] =
      s.pull.uncons.flatMap {
        case Some((c, rest)) =>
          row(c, env.currentField, env.tail, env.state, env.idx, env.start, env.current).flatMap(go(rest, _))
        case None =>
          // we're done parsing, emit potential last line
          env.state match {
            case State.BeginningOfField =>
              if (env.tail.nonEmpty)
                Pull.output1((NonEmptyList("", env.tail).reverse, env.start, env.current)) >> Pull.done
              else
                Pull.done
            case State.InUnquoted | State.InQuotedSeenQuote | State.ExpectNewLine =>
              Pull.output1((NonEmptyList(env.currentField.result, env.tail).reverse, env.start, env.current)) >> Pull.done
            case State.InUnquotedSeenCr =>
              Pull.output1(
                (NonEmptyList(env.currentField.append('\r').result, env.tail).reverse, env.start, env.current)) >>
                Pull.done
            case State.InQuoted =>
              Pull.raiseError[F](new ParseError("unexpected end of input", env.current))
          }
      }

    s => go(s, ParseEnv(new StringBuilder, Nil, State.BeginningOfField, 0, 1L, 1L)).stream
  }

}
