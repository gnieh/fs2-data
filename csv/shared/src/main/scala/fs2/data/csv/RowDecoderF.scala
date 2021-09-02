/*
 * Copyright 2021 Lucas Satabin
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
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._

import scala.annotation.tailrec

/** Describes how a row can be decoded to the given type.
  *
  * `RowDecoderF` provides convenient methods such as `map`, `emap`, or `flatMap`
  * to build new decoders out of more basic one.
  *
  * Actually, `RowDecoderF` has a [[https://typelevel.org/cats/api/cats/MonadError.html cats `MonadError`]]
  * instance. To get the full power of it, import `cats.implicits._`.
  */
@FunctionalInterface trait RowDecoderF[H[+a] <: Option[a], T, Header] {
  def apply(row: RowF[H, Header]): DecoderResult[T]

  /** Map the parsed value.
    * @param f the mapping function
    * @tparam T2 the result type
    * @return a row decoder reading the mapped type
    */
  def map[T2](f: T => T2): RowDecoderF[H, T2, Header] =
    row => apply(row).map(f)

  /** Map the parsed value to a new decoder, which in turn will be applied to
    * the parsed value.
    * @param f the mapping function
    * @tparam T2 the result type
    * @return a row decoder reading the mapped type
    */
  def flatMap[T2](f: T => RowDecoderF[H, T2, Header]): RowDecoderF[H, T2, Header] =
    row => apply(row).flatMap(f(_)(row))

  /** Map the parsed value, potentially failing.
    * @param f the mapping function
    * @tparam T2 the result type
    * @return a row decoder reading the mapped type
    */
  def emap[T2](f: T => DecoderResult[T2]): RowDecoderF[H, T2, Header] =
    row => apply(row).flatMap(f)

  /** Fail-over. If this decoder fails, try the supplied other decoder.
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

  /** Similar to [[or]], but return the result as an Either signaling which row decoder succeeded. Allows for parsing
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

object RowDecoderF extends ExportedRowDecoderFs {

  implicit def identityRowDecoderF[H[+a] <: Option[a], Header]: RowDecoderF[H, RowF[H, Header], Header] = _.asRight

  implicit def RowDecoderFInstances[H[+a] <: Option[a], Header]
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

  implicit def toMapCsvRowDecoder[Header]: CsvRowDecoder[Map[Header, String], Header] =
    CsvRowDecoder.instance(_.toMap.asRight)

  implicit def toNonEmptyMapCsvRowDecoder[Header: Order]: CsvRowDecoder[NonEmptyMap[Header, String], Header] =
    CsvRowDecoder.instance(_.toNonEmptyMap.asRight)

  implicit val toListRowDecoder: RowDecoder[List[String]] =
    RowDecoder.instance(_.values.toList.asRight)

  implicit val toNelRowDecoder: RowDecoder[NonEmptyList[String]] =
    RowDecoder.instance(_.values.asRight)
}

trait ExportedRowDecoderFs {
  implicit def exportedRowDecoders[A](implicit exported: Exported[RowDecoder[A]]): RowDecoder[A] = exported.instance

  implicit def exportedCsvRowDecoders[A](implicit
      exported: Exported[CsvRowDecoder[A, String]]): CsvRowDecoder[A, String] = exported.instance
}
