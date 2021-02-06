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
import cats._
import csv.internals._
import cats.data._
import cats.implicits._

import scala.annotation.implicitNotFound

package object csv {

  /**
    * Higher kinded version of [[scala.None]]. Ignores the type param.
    */
  type NoneF[+A] = None.type

  /**
    * A CSV row without headers.
    */
  type Row = RowF[NoneF, Nothing]

  /** A CSV row with headers, that can be used to access the cell values.
    *
    * '''Note:''' the following invariant holds when using this class: `values` and `headers` have the same size.
    */
  type CsvRow[Header] = RowF[Some, Header]

  type HeaderResult[T] = Either[HeaderError, NonEmptyList[T]]

  type DecoderResult[T] = Either[DecoderError, T]

  /** Describes how a row can be decoded to the given type.
    *
    * `RowDecoder` provides convenient methods such as `map`, `emap`, or `flatMap`
    * to build new decoders out of more basic one.
    *
    * Actually, `RowDecoder` has a [[https://typelevel.org/cats/api/cats/MonadError.html cats `MonadError`]]
    * instance. To get the full power of it, import `cats.implicits._`.
    */
  @implicitNotFound(
    "No implicit RowDecoder found for type ${T}.\nYou can define one using RowDecoder.instance, by calling map on another RowDecoder or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val rowDecoder: RowDecoder[${T}] = deriveRowDecoder\nMake sure to have instances of CellDecoder for every member type in scope.\n")
  type RowDecoder[T] = RowDecoderF[NoneF, T, Nothing]

  /** Describes how a row can be encoded from a value of the given type.
    */
  @implicitNotFound(
    "No implicit RowEncoder found for type ${T}.\nYou can define one using RowEncoder.instance, by calling contramap on another RowEncoder or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val rowEncoder: RowEncoder[${T}] = deriveRowEncoder\nMake sure to have instances of CellEncoder for every member type in scope.\n")
  type RowEncoder[T] = RowEncoderF[NoneF, T, Nothing]

  /** Describes how a row can be decoded to the given type.
    *
    * `CsvRowDecoder` provides convenient methods such as `map`, `emap`, or `flatMap`
    * to build new decoders out of more basic one.
    *
    * Actually, `CsvRowDecoder` has a [[https://typelevel.org/cats/api/cats/MonadError.html cats `MonadError`]]
    * instance. To get the full power of it, import `cats.implicits._`.
    */
  @implicitNotFound(
    "No implicit CsvRowDecoder found for type ${T}.\nYou can define one using CsvRowDecoder.instance, by calling map on another CsvRowDecoder or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val csvRowDecoder: CsvRowDecoder[${T}] = deriveCsvRowDecoder\nMake sure to have instances of CellDecoder for every member type in scope.\n")
  type CsvRowDecoder[T, Header] = RowDecoderF[Some, T, Header]

  /** Describes how a row can be encoded from a value of the given type.
    */
  @implicitNotFound(
    "No implicit CsvRowEncoderF[H,  found for type ${T}.\nYou can define one using CsvRowEncoderF[H, .instance, by calling contramap on another CsvRowEncoderF[H,  or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val csvRowEncoder: CsvRowEncoderF[H, [${T}] = deriveCsvRowEncoderF[H, \nMake sure to have instances of CellEncoder for every member type in scope.\n")
  type CsvRowEncoder[T, Header] = RowEncoderF[Some, T, Header]

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
  def rows[F[_], T](separator: Char = ',', quoteHandling: QuoteHandling = QuoteHandling.RFCCompliant)(implicit
      F: RaiseThrowable[F],
      T: CharLikeChunks[F, T]): Pipe[F, T, NonEmptyList[String]] =
    RowParser.pipe[F, T](separator, quoteHandling)

  /** Transforms a stream of raw CSV rows into parsed CSV rows with headers. */
  def headers[F[_], Header](implicit
      F: RaiseThrowable[F],
      Header: ParseableHeader[Header]): Pipe[F, NonEmptyList[String], CsvRow[Header]] =
    CsvRowParser.pipe[F, Header]

  /** Transforms a stream of raw CSV rows into parsed CSV rows with given headers. */
  def withHeaders[F[_], Header](headers: NonEmptyList[Header])(implicit
      F: RaiseThrowable[F]): Pipe[F, NonEmptyList[String], CsvRow[Header]] =
    _.map(CsvRow(_, headers)).rethrow

  /** Transforms a stream of raw CSV rows into rows. */
  def noHeaders[F[_]]: Pipe[F, NonEmptyList[String], Row] =
    _.map(Row(_))

  /** Transforms a stream of raw CSV rows into rows, skipping the first row to ignore the headers. */
  def skipHeaders[F[_]]: Pipe[F, NonEmptyList[String], Row] =
    _.tail.map(Row(_))

  def decode[F[_], R](implicit F: RaiseThrowable[F], R: RowDecoder[R]): Pipe[F, Row, R] =
    _.map(R(_)).rethrow

  def attemptDecode[F[_], R](implicit R: RowDecoder[R]): Pipe[F, Row, DecoderResult[R]] =
    _.map(R(_))

  def decodeRow[F[_], Header, R](implicit
      F: RaiseThrowable[F],
      R: CsvRowDecoder[R, Header]): Pipe[F, CsvRow[Header], R] =
    _.map(R(_)).rethrow

  def attemptDecodeRow[F[_], Header, R](implicit
      R: CsvRowDecoder[R, Header]): Pipe[F, CsvRow[Header], DecoderResult[R]] =
    _.map(R(_))

  def writeWithHeaders[F[_], Header](headers: NonEmptyList[Header])(implicit
      H: WriteableHeader[Header]): Pipe[F, Row, NonEmptyList[String]] =
    Stream(H(headers)) ++ _.map(_.values)

  def writeWithoutHeaders[F[_]]: Pipe[F, Row, NonEmptyList[String]] =
    _.map(_.values)

  def toStrings[F[_]](separator: Char = ',',
                      newline: String = "\n",
                      escape: EscapeMode = EscapeMode.Auto): Pipe[F, NonEmptyList[String], String] = {
    _.flatMap(nel =>
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
    _.map(R(_))

  def encodeRow[F[_], Header, R](implicit R: CsvRowEncoder[R, Header]): Pipe[F, R, CsvRow[Header]] =
    _.map(R(_))

  def encodeRowWithFirstHeaders[F[_], Header](implicit
      H: WriteableHeader[Header]): Pipe[F, CsvRow[Header], NonEmptyList[String]] =
    _.pull.peek1.flatMap {
      case Some((CsvRow(_, headers), stream)) =>
        Pull.output1(H(headers)) >> stream.map(_.values).pull.echo
      case None => Pull.done
    }.stream

}
