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
import cats.Show
import csv.internals._
import cats.data._
import cats.implicits._

package object csv {

  type HeaderResult[T] = Either[HeaderError, NonEmptyList[T]]

  type DecoderResult[T] = Either[DecoderError, T]

  sealed trait QuoteHandling
  object QuoteHandling {

    /** Treats quotation marks as the start of a quoted value if the first
      * character of a value is a quotation mark, otherwise treats the value
      * literally (this is the historic and default behavior)
      *
      * For example, "hello, world" would be parsed as unquoted `hello, world`
      */
    case object RFCCompliant extends QuoteHandling

    /** Treats values as raw strings and does not treat quotation marks with
      * any particular meaning
      *
      * For example, "hello, world" would be parsed as the still-quoted
      * `"hello, world"`
      */
    case object Literal extends QuoteHandling
  }

  /** Transforms a stream of characters into a stream of CSV rows.
    *
    * @param separator character to use to separate fields in the CSV
    * @param quoteHandling use [[QuoteHandling.RFCCompliant]] for RFC-4180
    *                      handling of quotation marks (optionally quoted
    *                      if the value begins with a quotation mark; the
    *                      default) or [[QuoteHandling.Literal]] if quotation
    *                      marks should be treated literally
    */
  def rows[F[_], T](separator: Char = ',', quoteHandling: QuoteHandling = QuoteHandling.RFCCompliant)(
      implicit F: RaiseThrowable[F],
      T: CharLikeChunks[F, T]): Pipe[F, T, NonEmptyList[String]] =
    RowParser.pipe[F, T](separator, quoteHandling)

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

  def writeWithHeaders[F[_], Header](headers: NonEmptyList[Header])(
      implicit H: WriteableHeader[Header]): Pipe[F, Row, NonEmptyList[String]] =
    Stream(H(headers)) ++ _.map(_.values)

  def writeWithoutHeaders[F[_]]: Pipe[F, Row, NonEmptyList[String]] =
    _.map(_.values)

  def toStrings[F[_]](separator: Char = ',',
                      newline: String = "\n",
                      escape: EscapeMode = EscapeMode.Auto): Pipe[F, NonEmptyList[String], String] = {
    _.flatMap(
      nel =>
        Stream
          .emits(nel.toList)
          .map(RowWriter.encodeColumn(separator, escape))
          .intersperse(separator.toString) ++ Stream(newline))
  }

  def toRowStrings[F[_]](separator: Char = ',',
                         newline: String = "\n",
                         escape: EscapeMode = EscapeMode.Auto): Pipe[F, NonEmptyList[String], String] = {
    // explicit Show avoids mapping the NEL before
    val showColumn: Show[String] =
      Show.show(RowWriter.encodeColumn(separator, escape))
    _.map(_.mkString_("", separator.toString, newline)(showColumn, implicitly))
  }

  def encode[F[_], R](implicit R: RowEncoder[R]): Pipe[F, R, Row] =
    _.map(row => new Row(R(row)))

  def encodeRow[F[_], Header, R](implicit R: CsvRowEncoder[R, Header]): Pipe[F, R, CsvRow[Header]] =
    _.map(R(_))

  def encodeRowWithFirstHeaders[F[_], Header](
      implicit H: WriteableHeader[Header]): Pipe[F, CsvRow[Header], NonEmptyList[String]] =
    _.pull.peek1.flatMap {
      case Some((CsvRow(_, headers), stream)) =>
        Pull.output1(H(headers)) >> stream.map(_.values).pull.echo
      case None => Pull.done
    }.stream

}
