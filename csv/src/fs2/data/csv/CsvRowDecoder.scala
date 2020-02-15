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
import cats.implicits._

import scala.annotation.tailrec

/** Describes how a row can be decoded to the given type.
  */
trait CsvRowDecoder[T, Header] {
  def apply(row: CsvRow[Header]): DecoderResult[T]
}

object CsvRowDecoder extends ExportedCsvRowDecoders {

  implicit def CsvRowDecoderMonadError[Header]: MonadError[CsvRowDecoder[*, Header], DecoderError] =
    new MonadError[CsvRowDecoder[*, Header], DecoderError] {
      def flatMap[A, B](fa: CsvRowDecoder[A, Header])(f: A => CsvRowDecoder[B, Header]): CsvRowDecoder[B, Header] =
        row => fa(row).flatMap(f(_)(row))

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
    }

  implicit def RowDecoderCsvRowDecoder[T](implicit T: RowDecoder[T]): CsvRowDecoder[T, Nothing] =
    new CsvRowDecoder[T, Nothing] {
      def apply(row: CsvRow[Nothing]): DecoderResult[T] =
        T(row.values)
    }

  def apply[T: CsvRowDecoder[*, Header], Header]: CsvRowDecoder[T, Header] = implicitly[CsvRowDecoder[T, Header]]

  def fromNonEmptyHeader[Header, T](decode: (NonEmptyList[String], Header) => DecoderResult[T]): CsvRowDecoder[T, Header] =
    (row: CsvRow[Header]) => row.header.toRight(new DecoderError("This decoder requires the header to be parsed"))
      .flatMap(decode(row.values, _))

}

trait ExportedCsvRowDecoders {
  implicit def exportedCsvRowDecoders[A](implicit exported: Exported[CsvRowDecoder[A, String]]): CsvRowDecoder[A, String] = exported.instance
}
