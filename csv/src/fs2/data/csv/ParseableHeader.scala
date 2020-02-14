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

import cats.{MonadError, SemigroupK}
import cats.implicits._

import scala.annotation.tailrec

/** A type class describing what it means to be a parseable
  * CSV header.
  */
trait ParseableHeader[Header] {

  def apply(name: String): DecoderResult[Header]

}

object ParseableHeader {

  def apply[Header: ParseableHeader]: ParseableHeader[Header] =
    implicitly[ParseableHeader[Header]]

  implicit object NothingParseableHeader extends ParseableHeader[Nothing] {
    def apply(name: String): DecoderResult[Nothing] = new DecoderError("no headers are expected").asLeft
  }

  implicit object StringParseableHeader extends ParseableHeader[String] {
    def apply(name: String): DecoderResult[String] = name.asRight
  }

  implicit object NonEmptyStringParseableHeader extends ParseableHeader[Option[String]] {
    def apply(name: String): DecoderResult[Option[String]] =
      if (name.isEmpty)
        none.asRight
      else
        name.some.asRight
  }

  implicit object ParseableHeaderInstances extends MonadError[ParseableHeader, DecoderError] with SemigroupK[ParseableHeader] {
    def flatMap[A, B](fa: ParseableHeader[A])(f: A => ParseableHeader[B]): ParseableHeader[B] =
      s => fa(s).flatMap(f(_)(s))

    def handleErrorWith[A](fa: ParseableHeader[A])(f: DecoderError => ParseableHeader[A]): ParseableHeader[A] =
      s => fa(s).leftFlatMap(f(_)(s))

    def pure[A](x: A): ParseableHeader[A] =
      _ => Right(x)

    def raiseError[A](e: DecoderError): ParseableHeader[A] =
      _ => Left(e)

    def tailRecM[A, B](a: A)(f: A => ParseableHeader[Either[A, B]]): ParseableHeader[B] = {
      @tailrec
      def step(s: String, a: A): DecoderResult[B] =
        f(a)(s) match {
          case left @ Left(_)          => left.rightCast[B]
          case Right(Left(a))          => step(s, a)
          case Right(right @ Right(_)) => right.leftCast[DecoderError]
        }
      s => step(s, a)
    }

    override def combineK[A](x: ParseableHeader[A], y: ParseableHeader[A]): ParseableHeader[A] = s =>
      x(s) match {
        case Left(_)      => y(s)
        case r @ Right(_) => r
      }
  }

}
