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

package fs2.data.csv

import cats.data.NonEmptyMap
import cats.syntax.all._
import cats.{MonadError, Order, SemigroupK}

import scala.annotation.{implicitNotFound, tailrec}

/** Describes how a row can be decoded to the given type.
  *
  * `CsvRowDecoder` provides convenient methods such as `map`, `emap`, or `flatMap`
  * to build new decoders out of more basic one.
  *
  * Actually, `CsvRowDecoder` has a [[https://typelevel.org/cats/api/cats/MonadError.html cats `MonadError`]]
  * instance. To get the full power of it, import `cats.syntax.all._`.
  */
@implicitNotFound(
  "No implicit CsvRowDecoder found for type ${T}.\nYou can define one using CsvRowDecoder.instance, by calling map on another CsvRowDecoder or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val csvRowDecoder: CsvRowDecoder[${T}] = deriveCsvRowDecoder\nMake sure to have instances of CellDecoder for every member type in scope.\n")
@FunctionalInterface trait CsvRowDecoder[T, Header] {
  def apply(row: CsvRow[Header]): DecoderResult[T]

  /** Map the parsed value.
    *
    * @param f the mapping function
    * @tparam T2 the result type
    * @return a row decoder reading the mapped type
    */
  def map[T2](f: T => T2): CsvRowDecoder[T2, Header] =
    row => apply(row).map(f)

  /** Map the parsed value to a new decoder, which in turn will be applied to
    * the parsed value.
    *
    * @param f the mapping function
    * @tparam T2 the result type
    * @return a row decoder reading the mapped type
    */
  def flatMap[T2](f: T => CsvRowDecoder[T2, Header]): CsvRowDecoder[T2, Header] =
    row => apply(row).flatMap(f(_)(row))

  /** Map the parsed value, potentially failing.
    *
    * @param f the mapping function
    * @tparam T2 the result type
    * @return a row decoder reading the mapped type
    */
  def emap[T2](f: T => DecoderResult[T2]): CsvRowDecoder[T2, Header] =
    row => apply(row).flatMap(f)

  /** Fail-over. If this decoder fails, try the supplied other decoder.
    *
    * @param cd the fail-over decoder
    * @tparam TT the return type
    * @return a decoder combining this and the other decoder
    */
  def or[TT >: T](cd: => CsvRowDecoder[TT, Header]): CsvRowDecoder[TT, Header] =
    row =>
      apply(row) match {
        case Left(_)      => cd(row)
        case r @ Right(_) => r.leftCast[DecoderError]
      }

  /** Similar to [[or]], but return the result as an Either signaling which row decoder succeeded. Allows for parsing
    * an unrelated type in case of failure.
    *
    * @param cd the alternative decoder
    * @tparam B the type the alternative decoder returns
    * @return a decoder combining both decoders
    */
  def either[B](cd: CsvRowDecoder[B, Header]): CsvRowDecoder[Either[T, B], Header] =
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

object CsvRowDecoder extends ExportedCsvRowDecoders {

  @inline
  def apply[T: CsvRowDecoder[*, Header], Header]: CsvRowDecoder[T, Header] = implicitly[CsvRowDecoder[T, Header]]

  @inline
  def instance[T, Header](f: CsvRow[Header] => DecoderResult[T]): CsvRowDecoder[T, Header] = f(_)

  implicit def identityCsvRowDecoder[Header]: CsvRowDecoder[CsvRow[Header], Header] = _.asRight

  implicit def CsvRowDecoderInstances[Header]
      : MonadError[CsvRowDecoder[*, Header], DecoderError] with SemigroupK[CsvRowDecoder[*, Header]] =
    new MonadError[CsvRowDecoder[*, Header], DecoderError] with SemigroupK[CsvRowDecoder[*, Header]] {
      override def map[A, B](fa: CsvRowDecoder[A, Header])(f: A => B): CsvRowDecoder[B, Header] =
        fa.map(f)

      def flatMap[A, B](fa: CsvRowDecoder[A, Header])(f: A => CsvRowDecoder[B, Header]): CsvRowDecoder[B, Header] =
        fa.flatMap(f)

      def handleErrorWith[A](fa: CsvRowDecoder[A, Header])(
          f: DecoderError => CsvRowDecoder[A, Header]): CsvRowDecoder[A, Header] =
        row => fa(row).leftFlatMap(f(_)(row))

      def pure[A](x: A): CsvRowDecoder[A, Header] =
        _ => Right(x)

      def raiseError[A](e: DecoderError): CsvRowDecoder[A, Header] =
        _ => Left(e)

      def tailRecM[A, B](a: A)(f: A => CsvRowDecoder[Either[A, B], Header]): CsvRowDecoder[B, Header] = {
        @tailrec
        def step(row: CsvRow[Header], a: A): DecoderResult[B] =
          f(a)(row) match {
            case left @ Left(_)          => left.rightCast[B]
            case Right(Left(a))          => step(row, a)
            case Right(right @ Right(_)) => right.leftCast[DecoderError]
          }

        row => step(row, a)
      }

      def combineK[A](x: CsvRowDecoder[A, Header], y: CsvRowDecoder[A, Header]): CsvRowDecoder[A, Header] = x or y
    }

  implicit def toMapCsvRowDecoder[Header]: CsvRowDecoder[Map[Header, String], Header] =
    CsvRowDecoder.instance(_.toMap.asRight)

  implicit def toNonEmptyMapCsvRowDecoder[Header: Order]: CsvRowDecoder[NonEmptyMap[Header, String], Header] =
    CsvRowDecoder.instance(_.toNonEmptyMap.asRight)

  implicit def decodeResultCsvRowDecoder[Header, T](implicit
      dec: CsvRowDecoder[T, Header]): CsvRowDecoder[DecoderResult[T], Header] =
    r => Right(dec(r))

}

trait ExportedCsvRowDecoders {

  implicit def exportedCsvRowDecoders[A](implicit
      exported: Exported[CsvRowDecoder[A, String]]): CsvRowDecoder[A, String] = exported.instance
}
