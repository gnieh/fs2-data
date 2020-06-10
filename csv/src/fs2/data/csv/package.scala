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

import text._
import csv.internals._

import cats.data._

package object csv {

  type HeaderResult[T] = Either[HeaderError, NonEmptyList[T]]

  type DecoderResult[T] = Either[DecoderError, T]

  /** Transforms a stream of characters into a stream of CSV rows.
    */
  def rows[F[_], T](separator: Char = ',')(implicit F: RaiseThrowable[F],
                                           T: CharLikeChunks[F, T]): Pipe[F, T, NonEmptyList[String]] =
    RowParser.pipe[F, T](separator)

  /** Transforms a stream of raw CSV rows into parsed CSV rows with headers. */
  def headers[F[_], Header](implicit F: RaiseThrowable[F],
                            Header: ParseableHeader[Header]): Pipe[F, NonEmptyList[String], CsvRow[Header]] =
    CsvRowParser.pipe[F, Header]

  /** Transforms a stream of raw CSV rows into parsed CSV rows with given headers. */
  def withHeaders[F[_], Header](headers: NonEmptyList[Header]): Pipe[F, NonEmptyList[String], CsvRow[Header]] =
    _.map(CsvRow(_, headers))

  /** Transforms a stream of raw CSV rows into rows. */
  def noHeaders[F[_]]: Pipe[F, NonEmptyList[String], Row] =
    _.map(new Row(_))

  /** Transforms a stream of raw CSV rows into rows, skipping the first row to ignore the headers. */
  def skipHeaders[F[_]]: Pipe[F, NonEmptyList[String], Row] =
    _.tail.map(new Row(_))

  def decode[F[_], R](implicit F: RaiseThrowable[F], R: RowDecoder[R]): Pipe[F, Row, R] =
    _.map(row => R(row.values)).rethrow

  def attemptDecode[F[_], R](implicit R: RowDecoder[R]): Pipe[F, Row, DecoderResult[R]] =
    _.map(row => R(row.values))

  def decodeRow[F[_], Header, R](implicit F: RaiseThrowable[F],
                                 R: CsvRowDecoder[R, Header]): Pipe[F, CsvRow[Header], R] =
    _.map(R(_)).rethrow

  def attemptDecodeRow[F[_], Header, R](
      implicit R: CsvRowDecoder[R, Header]): Pipe[F, CsvRow[Header], DecoderResult[R]] =
    _.map(R(_))

}
