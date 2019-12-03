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
import cats.implicits._
import cats.data.NonEmptyList

import scala.annotation.tailrec

/** Describes how a row can be decoded to the given type.
  */
trait RowDecoder[T] {
  def apply(cells: NonEmptyList[String]): DecoderResult[T]
}

object RowDecoder {

  implicit object RowDecoderMonadError extends MonadError[RowDecoder, DecoderError] {
    def flatMap[A, B](fa: RowDecoder[A])(f: A => RowDecoder[B]): RowDecoder[B] =
      cells => fa(cells).flatMap(f(_)(cells))

    def handleErrorWith[A](fa: RowDecoder[A])(f: DecoderError => RowDecoder[A]): RowDecoder[A] =
      cells => fa(cells).leftFlatMap(f(_)(cells))

    def pure[A](x: A): RowDecoder[A] =
      _ => Right(x)

    def raiseError[A](e: DecoderError): RowDecoder[A] =
      _ => Left(e)

    def tailRecM[A, B](a: A)(f: A => RowDecoder[Either[A, B]]): RowDecoder[B] = {
      @tailrec
      def step(cells: NonEmptyList[String], a: A): DecoderResult[B] =
        f(a)(cells) match {
          case left @ Left(_)          => left.rightCast[B]
          case Right(Left(a))          => step(cells, a)
          case Right(right @ Right(_)) => right.leftCast[DecoderError]
        }
      cells => step(cells, a)
    }

  }

  def apply[T: RowDecoder]: RowDecoder[T] = implicitly[RowDecoder[T]]

}
