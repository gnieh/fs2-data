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

import scala.annotation.{implicitNotFound, tailrec}

package object csv {

  trait RowDecoderF[H[a] <: Option[a], T, Header] {
    def apply(row: RowF[H, Header]): DecoderResult[T]

    /**
      * Map the parsed value.
      * @param f the mapping function
      * @tparam T2 the result type
      * @return a row decoder reading the mapped type
      */
    def map[T2](f: T => T2): RowDecoderF[H, T2, Header] =
      row => apply(row).map(f)

    /**
      * Map the parsed value to a new decoder, which in turn will be applied to
      * the parsed value.
      * @param f the mapping function
      * @tparam T2 the result type
      * @return a row decoder reading the mapped type
      */
    def flatMap[T2](f: T => RowDecoderF[H, T2, Header]): RowDecoderF[H, T2, Header] =
      row => apply(row).flatMap(f(_)(row))

    /**
      * Map the parsed value, potentially failing.
      * @param f the mapping function
      * @tparam T2 the result type
      * @return a row decoder reading the mapped type
      */
    def emap[T2](f: T => DecoderResult[T2]): RowDecoderF[H, T2, Header] =
      row => apply(row).flatMap(f)

    /**
      * Fail-over. If this decoder fails, try the supplied other decoder.
      * @param cd the fail-over decoder
      * @tparam TT the return type
      * @return a decoder combining this and the other decoder
      */
    def or[TT >: T](cd: => RowDecoderF[H, TT, Header]): RowDecoderF[H, TT, Header] =
      row =>
        apply(row) match {
          case Left(_)      => cd(row)
          case r @ Right(_) => r.leftCast[DecoderError]
        }

    /**
      * Similar to [[or]], but return the result as an Either signaling which row decoder succeeded. Allows for parsing
      * an unrelated type in case of failure.
      * @param cd the alternative decoder
      * @tparam B the type the alternative decoder returns
      * @return a decoder combining both decoders
      */
    def either[B](cd: RowDecoderF[H, B, Header]): RowDecoderF[H, Either[T, B], Header] =
      row =>
        apply(row) match {
          case Left(_) =>
            cd(row) match {
              case l @ Left(_)  => l.rightCast[Either[T, B]]
              case r @ Right(_) => r.leftCast[T].asRight
            }
          case Right(value) => Right(Left(value))
        }
  }

  object RowDecoderF extends ExportedCsvRowDecoders with ExportedRowDecoders {

    implicit def RowDecoderFInstances[H[a] <: Option[a], Header]
        : MonadError[RowDecoderF[H, *, Header], DecoderError] with SemigroupK[RowDecoderF[H, *, Header]] =
      new MonadError[RowDecoderF[H, *, Header], DecoderError] with SemigroupK[RowDecoderF[H, *, Header]] {
        override def map[A, B](fa: RowDecoderF[H, A, Header])(f: A => B): RowDecoderF[H, B, Header] =
          fa.map(f)

        def flatMap[A, B](fa: RowDecoderF[H, A, Header])(f: A => RowDecoderF[H, B, Header]): RowDecoderF[H, B, Header] =
          fa.flatMap(f)

        def handleErrorWith[A](fa: RowDecoderF[H, A, Header])(
            f: DecoderError => RowDecoderF[H, A, Header]): RowDecoderF[H, A, Header] =
          row => fa(row).leftFlatMap(f(_)(row))

        def pure[A](x: A): RowDecoderF[H, A, Header] =
          _ => Right(x)

        def raiseError[A](e: DecoderError): RowDecoderF[H, A, Header] =
          _ => Left(e)

        def tailRecM[A, B](a: A)(f: A => RowDecoderF[H, Either[A, B], Header]): RowDecoderF[H, B, Header] = {
          @tailrec
          def step(row: RowF[H, Header], a: A): DecoderResult[B] =
            f(a)(row) match {
              case left @ Left(_)          => left.rightCast[B]
              case Right(Left(a))          => step(row, a)
              case Right(right @ Right(_)) => right.leftCast[DecoderError]
            }
          row => step(row, a)
        }

        def combineK[A](x: RowDecoderF[H, A, Header], y: RowDecoderF[H, A, Header]): RowDecoderF[H, A, Header] = x or y
      }

  }

  type NoneF[A] = None.type

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

  type CsvRowEncoder[T, Header] = RowEncoderF[Some, T, Header]

  type Row = RowF[NoneF, Nothing]
  object Row {
    def apply(values: NonEmptyList[String]): Row = new Row(values, None)
    def unapply(arg: Row): Some[NonEmptyList[String]] = Some(arg.values)
  }

  type CsvRow[Header] = RowF[Some, Header]

  object CsvRow {

    /** Constructs a [[CsvRow]] and checks that the size of values and headers match. */
    def apply[Header](values: NonEmptyList[String],
                      headers: NonEmptyList[Header]): Either[CsvException, CsvRow[Header]] =
      if (values.length =!= headers.length)
        Left(
          new CsvException(
            s"Headers have size ${headers.length} but row has size ${values.length}. Both numbers must match!"))
      else
        Right(new CsvRow(values, Some(headers)))

    def unsafe[Header](values: NonEmptyList[String], headers: NonEmptyList[Header]): CsvRow[Header] =
      apply(values, headers).fold(throw _, identity)

    def fromListHeaders[Header](l: List[(Header, String)]): Option[CsvRow[Header]] = {
      val (hs, vs) = l.unzip
      (NonEmptyList.fromList(vs), NonEmptyList.fromList(hs)).mapN((v, h) => new CsvRow(v, Some(h)))
    }

    def fromNelHeaders[Header](nel: NonEmptyList[(Header, String)]): CsvRow[Header] = {
      val (hs, vs) = nel.toList.unzip
      new CsvRow(NonEmptyList.fromListUnsafe(vs), Some(NonEmptyList.fromListUnsafe(hs)))
    }

    def unapply[Header](arg: CsvRow[Header]): Some[(NonEmptyList[String], NonEmptyList[Header])] = Some(
      (arg.values, arg.headers.get))
  }

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
