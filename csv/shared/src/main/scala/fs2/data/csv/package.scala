/*
 * Copyright 2024 fs2-data Project
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
import cats.syntax.all._

import scala.annotation.implicitNotFound
import scala.annotation.nowarn

package object csv {

  /** Higher kinded version of [[scala.None]]. Ignores the type param.
    */
  type NoneF[+A] = None.type

  /** A CSV row without headers.
    */
  type Row = RowF[NoneF]

  type HeaderResult[T] = Either[HeaderError, NonEmptyList[T]]

  type DecoderResult[T] = Either[DecoderError, T]

  /** Describes how a row can be decoded to the given type.
    *
    * `RowDecoder` provides convenient methods such as `map`, `emap`, or `flatMap`
    * to build new decoders out of more basic one.
    *
    * Actually, `RowDecoder` has a [[https://typelevel.org/cats/api/cats/MonadError.html cats `MonadError`]]
    * instance. To get the full power of it, import `cats.syntax.all._`.
    */
  @implicitNotFound(
    "No implicit RowDecoder found for type ${T}.\nYou can define one using RowDecoder.instance, by calling map on another RowDecoder or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val rowDecoder: RowDecoder[${T}] = deriveRowDecoder\nMake sure to have instances of CellDecoder for every member type in scope.\n")
  type RowDecoder[T] = RowDecoderF[NoneF, T]

  /** Describes how a row can be encoded from a value of the given type.
    */
  @implicitNotFound(
    "No implicit RowEncoder found for type ${T}.\nYou can define one using RowEncoder.instance, by calling contramap on another RowEncoder or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val rowEncoder: RowEncoder[${T}] = deriveRowEncoder\nMake sure to have instances of CellEncoder for every member type in scope.\n")
  type RowEncoder[T] = RowEncoderF[NoneF, T]

  @nowarn
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

  /** Decode a char-like stream (see [[fs2.data.text.CharLikeChunks]]) into a specified type,
    * assuming the file neither contains headers nor are they needed for decoding.
    */
  def decodeWithoutHeaders[T]: PartiallyAppliedDecodeWithoutHeaders[T] =
    new PartiallyAppliedDecodeWithoutHeaders[T](dummy = true)

  @nowarn
  class PartiallyAppliedDecodeWithoutHeaders[T](val dummy: Boolean) extends AnyVal {
    def apply[F[_], C](separator: Char = ',', quoteHandling: QuoteHandling = QuoteHandling.RFCCompliant)(implicit
        F: RaiseThrowable[F],
        C: CharLikeChunks[F, C],
        T: RowDecoder[T]): Pipe[F, C, T] =
      lowlevel.rows(separator, quoteHandling) andThen lowlevel.noHeaders andThen lowlevel.decode
  }

  /** Decode a char-like stream (see [[fs2.data.text.CharLikeChunks]]) into a specified type,
    * assuming the file contains headers, but they shall be skipped for decoding.
    */
  def decodeSkippingHeaders[T]: PartiallyAppliedDecodeSkippingHeaders[T] =
    new PartiallyAppliedDecodeSkippingHeaders[T](dummy = true)

  @nowarn
  class PartiallyAppliedDecodeSkippingHeaders[T](val dummy: Boolean) extends AnyVal {
    def apply[F[_], C](separator: Char = ',', quoteHandling: QuoteHandling = QuoteHandling.RFCCompliant)(implicit
        F: RaiseThrowable[F],
        C: CharLikeChunks[F, C],
        T: RowDecoder[T]
    ): Pipe[F, C, T] =
      lowlevel.rows(separator, quoteHandling) andThen lowlevel.skipHeaders andThen lowlevel.decode
  }

  /** Encode a specified type into a CSV that contains no headers. */
  def encodeWithoutHeaders[T]: PartiallyAppliedEncodeWithoutHeaders[T] =
    new PartiallyAppliedEncodeWithoutHeaders[T](dummy = true)

  @nowarn
  class PartiallyAppliedEncodeWithoutHeaders[T](val dummy: Boolean) extends AnyVal {
    def apply[F[_]](fullRows: Boolean = false,
                    separator: Char = ',',
                    newline: String = "\n",
                    escape: EscapeMode = EscapeMode.Auto)(implicit T: RowEncoder[T]): Pipe[F, T, String] = {
      val stringPipe =
        if (fullRows) lowlevel.toRowStrings[F](separator, newline, escape)
        else lowlevel.toStrings[F](separator, newline, escape)
      lowlevel.encode[F, T] andThen lowlevel.writeWithoutHeaders andThen stringPipe
    }
  }

  /** Encode a specified type into a CSV prepending the given headers. */
  def encodeGivenHeaders[T]: PartiallyAppliedEncodeGivenHeaders[T] =
    new PartiallyAppliedEncodeGivenHeaders[T](dummy = true)

  @nowarn
  class PartiallyAppliedEncodeGivenHeaders[T](val dummy: Boolean) extends AnyVal {
    def apply[F[_], Header](headers: NonEmptyList[Header],
                            fullRows: Boolean = false,
                            separator: Char = ',',
                            newline: String = "\n",
                            escape: EscapeMode = EscapeMode.Auto)(implicit
        T: RowEncoder[T],
        H: WriteableHeader[Header]): Pipe[F, T, String] = {
      val stringPipe =
        if (fullRows) lowlevel.toRowStrings[F](separator, newline, escape)
        else lowlevel.toStrings[F](separator, newline, escape)
      lowlevel.encode[F, T] andThen lowlevel.writeWithHeaders(headers) andThen stringPipe
    }
  }

  /** Low level pipes for CSV handling. All pipes only perform one step in a CSV (de)serialization pipeline,
    * so use these if you want to customise. All standard use cases should be covered by the higher level pipes directly
    * on the csv package which are composed of the lower level ones here.
    */
  object lowlevel {

    /** Transforms a stream of characters into a stream of CSV rows.
      *
      * @param separator     character to use to separate fields in the CSV
      * @param quoteHandling use [[QuoteHandling.RFCCompliant]] for RFC-4180
      *                      handling of quotation marks (optionally quoted
      *                      if the value begins with a quotation mark; the
      *                      default) or [[QuoteHandling.Literal]] if quotation
      *                      marks should be treated literally
      */
    def rows[F[_], T](separator: Char = ',', quoteHandling: QuoteHandling = QuoteHandling.RFCCompliant)(implicit
        F: RaiseThrowable[F],
        T: CharLikeChunks[F, T]): Pipe[F, T, Row] =
      RowParser.pipe[F, T](separator, quoteHandling)

    /** Transforms a stream of raw CSV rows into rows. */
    def noHeaders[F[_]]: Pipe[F, Row, Row] = identity

    /** Transforms a stream of raw CSV rows into rows, skipping the first row to ignore the headers. */
    def skipHeaders[F[_]]: Pipe[F, Row, Row] =
      _.tail

    /** Decodes simple rows (without headers) into a specified type using a suitable [[RowDecoder]]. */
    def decode[F[_], R](implicit F: RaiseThrowable[F], R: RowDecoder[R]): Pipe[F, Row, R] =
      _.map(R(_)).rethrow

    /** Decodes simple rows (without headers) into a specified type using a suitable [[RowDecoder]], but signal errors as values. */
    def attemptDecode[F[_], R](implicit R: RowDecoder[R]): Pipe[F, Row, DecoderResult[R]] =
      _.map(R(_))

    /** Encode a given type into CSV rows using a set of explicitly given headers. */
    def writeWithHeaders[F[_], Header](headers: NonEmptyList[Header])(implicit
        H: WriteableHeader[Header]): Pipe[F, Row, NonEmptyList[String]] =
      Stream(H(headers)) ++ _.map(_.values)

    /** Encode a given type into CSV rows without headers. */
    def writeWithoutHeaders[F[_]]: Pipe[F, Row, NonEmptyList[String]] =
      _.map(_.values)

    /** Serialize a CSV row to Strings. No guarantees are given on how the resulting Strings are cut. */
    def toStrings[F[_]](separator: Char = ',',
                        newline: String = "\n",
                        escape: EscapeMode = EscapeMode.Auto): Pipe[F, NonEmptyList[String], String] = {
      _.flatMap(nel =>
        Stream
          .emits(nel.toList)
          .map(RowWriter.encodeColumn(separator, escape))
          .intersperse(separator.toString) ++ Stream(newline))
    }

    /** Serialize a CSV row to Strings. Guaranteed to emit one String per CSV row (= one line if no quoted newlines are contained in the value). */
    def toRowStrings[F[_]](separator: Char = ',',
                           newline: String = "\n",
                           escape: EscapeMode = EscapeMode.Auto): Pipe[F, NonEmptyList[String], String] = {
      // explicit Show avoids mapping the NEL before
      val showColumn: Show[String] =
        Show.show(RowWriter.encodeColumn(separator, escape))
      _.map(_.mkString_("", separator.toString, newline)(showColumn, implicitly))
    }

    /** Encode a given type into simple CSV rows without headers. */
    def encode[F[_], R](implicit R: RowEncoder[R]): Pipe[F, R, Row] =
      _.map(R(_))
  }

  object lenient {
    /** Decode a char-like stream (see [[fs2.data.text.CharLikeChunks]]) into a specified type, with failures at the
      * element level instead of failing the stream.
      *
      * This function assumes the file contains headers, but they shall be skipped for decoding.
      */
    def attemptDecodeSkippingHeaders[T]: PartiallyAppliedDecodeAttemptSkippingHeaders[T] =
      new PartiallyAppliedDecodeAttemptSkippingHeaders[T](dummy = true)

    class PartiallyAppliedDecodeAttemptSkippingHeaders[T](val dummy: Boolean) extends AnyVal {
      def apply[F[_], C](separator: Char = ',', quoteHandling: QuoteHandling = QuoteHandling.RFCCompliant)(implicit
          F: RaiseThrowable[F],
          C: CharLikeChunks[F, C],
          T: RowDecoder[T]): Pipe[F, C, Either[CsvException, T]] = {
        lowlevel.rows(separator, quoteHandling) andThen lowlevel.skipHeaders andThen lowlevel.attemptDecode
      }
    }

    /** Decode a char-like stream (see [[fs2.data.text.CharLikeChunks]]) into a specified type, with failures at the
      * element level instead of failing the stream.
      *
      * This function assumes the file neither contains headers nor are they needed for decoding.
      */
    def attemptDecodeWithoutHeaders[T]: PartiallyAppliedDecodeAttemptWithoutHeaders[T] =
      new PartiallyAppliedDecodeAttemptWithoutHeaders[T](dummy = true)

    class PartiallyAppliedDecodeAttemptWithoutHeaders[T](val dummy: Boolean) extends AnyVal {
      def apply[F[_], C](separator: Char = ',', quoteHandling: QuoteHandling = QuoteHandling.RFCCompliant)(implicit
          F: RaiseThrowable[F],
          C: CharLikeChunks[F, C],
          T: RowDecoder[T]): Pipe[F, C, Either[CsvException, T]] =
        lowlevel.rows(separator, quoteHandling) andThen lowlevel.noHeaders andThen lowlevel.attemptDecode
    }
  }
}
