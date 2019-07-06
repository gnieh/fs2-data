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

  def withHeaders[Header: ParseableHeader](implicit F: MonadError[F, Throwable]): Pipe[F, Byte, CsvRow[Header]] =
    CsvParser.fromBytes[F, Header](true, separator)

  def noHeaders(implicit F: MonadError[F, Throwable]): Pipe[F, Byte, CsvRow[Nothing]] =
    CsvParser.fromBytes[F, Nothing](false, separator)

}

class ToStringCsvPipe[F[_]] private[csv] (val separator: Char) extends AnyVal {

  def withHeaders[Header: ParseableHeader](implicit F: MonadError[F, Throwable]): Pipe[F, String, CsvRow[Header]] =
    CsvParser.fromString[F, Header](true, separator)

  def noHeaders(implicit F: MonadError[F, Throwable]): Pipe[F, String, CsvRow[Nothing]] =
    CsvParser.fromString[F, Nothing](false, separator)

}

private object CsvParser {

  def fromBytes[F[_], Header](withHeaders: Boolean, separator: Char)(
      implicit F: MonadError[F, Throwable],
      H: ParseableHeader[Header]): Pipe[F, Byte, CsvRow[Header]] =
    _.through(text.utf8Decode)
      .through(fromString[F, Header](withHeaders, separator))

  def fromString[F[_], Header](withHeaders: Boolean, separator: Char)(
      implicit F: MonadError[F, Throwable],
      H: ParseableHeader[Header]): Pipe[F, String, CsvRow[Header]] = {
    _.through(rows[F](separator))
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
