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
package fs2.data.csv

import cats._
import cats.data.NonEmptyList

import scala.annotation.tailrec

import fs2._

import scala.language.higherKinds

class ToByteCsvPipe[F[_]] private[csv] (val separator: Char) extends AnyVal {

  def withHeaders[Header: ParseableHeader](implicit F: ApplicativeError[F, Throwable]): Pipe[F, Byte, CsvRow[Header]] =
    CsvParser.fromBytes[F, Header](true, separator)

  def noHeaders(implicit F: ApplicativeError[F, Throwable]): Pipe[F, Byte, CsvRow[Nothing]] =
    CsvParser.fromBytes[F, Nothing](false, separator)

}

class ToStringCsvPipe[F[_]] private[csv] (val separator: Char) extends AnyVal {

  def withHeaders[Header: ParseableHeader](
      implicit F: ApplicativeError[F, Throwable]): Pipe[F, String, CsvRow[Header]] =
    CsvParser.fromString[F, Header](true, separator)

  def noHeaders(implicit F: ApplicativeError[F, Throwable]): Pipe[F, String, CsvRow[Nothing]] =
    CsvParser.fromString[F, Nothing](false, separator)

}

private object CsvParser {

  def fromBytes[F[_], Header](withHeaders: Boolean, separator: Char = ',')(
      implicit F: ApplicativeError[F, Throwable],
      H: ParseableHeader[Header]): Pipe[F, Byte, CsvRow[Header]] =
    _.through(text.utf8Decode)
      .through(fromString[F, Header](withHeaders, separator))

  def fromString[F[_], Header](withHeaders: Boolean, separator: Char = ',')(
      implicit F: ApplicativeError[F, Throwable],
      H: ParseableHeader[Header]): Pipe[F, String, CsvRow[Header]] = {

    val rows: Pipe[F, String, NonEmptyList[String]] = {

      def go(s: Stream[F, String], prev: Option[ParseRowResult.ToBeContinued]): Pull[F, NonEmptyList[String], Unit] =
        s.pull.uncons1.flatMap {
          case Some((line, rest)) =>
            @tailrec
            def quoted(idx: Int, acc: StringBuilder): (String, Int, Boolean) =
              // read until next unescaped quote or end of line
              // if end of line is reached, mark the field as not terminated
              if (idx >= line.length)
                (acc.result, idx, false)
              else {
                val c = line.charAt(idx)
                if (c == '"')
                  // check whether the quote is escaped (two consecutive quotes)
                  if (idx + 1 < line.length && line.charAt(idx + 1) == '"')
                    // escaped quote
                    quoted(idx + 2, acc.append('"'))
                  else
                    (acc.result, idx + 1, true)
                else
                  quoted(idx + 1, acc.append(c))
              }

            @tailrec
            def unquoted(idx: Int, acc: StringBuilder): (String, Int, Boolean) =
              // read until separator or end of line
              if (idx >= line.length)
                (acc.result, idx, true)
              else {
                val c = line.charAt(idx)
                if (c == separator)
                  (acc.result, idx, true)
                else
                  unquoted(idx + 1, acc.append(c))
              }

            def field(idx: Int, acc: StringBuilder, cont: Boolean): (String, Int, Boolean) =
              if (idx >= line.length)
                (acc.result, idx, !cont)
              else {
                val c = line.charAt(idx)

                if (c == separator)
                  (acc.result, idx, true)
                else if (c == '"')
                  if (cont)
                    // this is the continuation of a quoted field, finish parsing it
                    quoted(idx, acc)
                  else
                    quoted(idx + 1, acc)
                else if (cont)
                  // continue the unclosed quoted field
                  quoted(idx, acc)
                else
                  unquoted(idx, acc)
              }

            def fields(idx: Int, acc: NonEmptyList[String]): Pull[F, NonEmptyList[String], Unit] =
              if (idx >= line.length)
                if (idx == 0)
                  // empty line, skip it
                  go(rest, None)
                else
                  Pull.output1(acc.reverse) >> go(rest, None)
              else {
                val c = line.charAt(idx)
                if (c == separator) {
                  field(idx + 1, new StringBuilder, false) match {
                    case (field, idx, true) =>
                      fields(idx, field :: acc)
                    case (field, _, false) =>
                      go(rest, Some(ParseRowResult.ToBeContinued(field, acc.toList)))
                  }
                } else {
                  Pull.raiseError(new CsvException(s"unexpected character '$c'"))
                }
              }

            val (head, tail, cont) = prev match {
              case Some(ParseRowResult.ToBeContinued(last, tail)) => (Some(last), tail, true)
              case None                                           => (None, Nil, false)
            }
            val builder = new StringBuilder
            head.foreach(builder.append(_).append("\n"))
            field(0, builder, cont) match {
              case (field, idx, true) =>
                fields(idx, NonEmptyList(field, tail))
              case (field, _, false) =>
                go(rest, Some(ParseRowResult.ToBeContinued(field, tail)))
            }
          case None =>
            prev match {
              case Some(_) => Pull.raiseError[F](new CsvException("unexpected end of input"))
              case None    => Pull.done
            }
        }
      s => go(s, None).stream
    }

    // string is split into lines because usually it will exactly match a csv row
    // however a row might span several lines if a field is quoted and the value contains
    // new line characters
    _.through(text.lines)
      .through(rows)
      .mapAccumulate(Headers.Uninitialized[Header]: Headers[Header]) {
        case (Headers.Uninitialized(), fields) if withHeaders =>
          // headers have not been seen yet (first row)
          (Headers.Initialized(Some(fields.map(ParseableHeader[Header].parse(_)))), None)
        case (Headers.Uninitialized(), fields) =>
          // no header are to be parsed
          (Headers.Initialized(None), Some(new CsvRow[Header](fields, None)))
        case (initialized @ Headers.Initialized(headers), fields) =>
          // otherwise, headers are already initialized properly, just pass them to the row
          (initialized, Some(new CsvRow[Header](fields, headers)))
      }
      .map(_._2)
      .unNone

  }
}
