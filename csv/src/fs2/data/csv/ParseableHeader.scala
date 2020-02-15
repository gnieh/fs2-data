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

import cats.data.NonEmptyList
import cats.{MonadError, SemigroupK}
import cats.implicits._

import scala.annotation.tailrec

/** A type class describing what it means to be a parseable
  * CSV header.
  */
trait ParseableHeader[Header] {

  def apply(names: NonEmptyList[String]): HeaderResult[Header]

}

object ParseableHeader {

  def apply[Header: ParseableHeader]: ParseableHeader[Header] =
    implicitly[ParseableHeader[Header]]

  def parseIndependently[HeadElem](parse: String => HeaderResult[HeadElem]): ParseableHeader[NonEmptyList[HeadElem]] =
    (names: NonEmptyList[String]) => names.traverse(parse)

  implicit object NothingParseableHeader extends ParseableHeader[Nothing] {
    def apply(names: NonEmptyList[String]): HeaderResult[Nothing] = new HeaderError("no headers are expected").asLeft
  }

  implicit object StringNelParseableHeader extends ParseableHeader[NonEmptyList[String]] {
    def apply(names: NonEmptyList[String]): HeaderResult[NonEmptyList[String]] = names.asRight
  }

  implicit object NonEmptyStringNelParseableHeader extends ParseableHeader[NonEmptyList[Option[String]]] {
    def apply(names: NonEmptyList[String]): HeaderResult[NonEmptyList[Option[String]]] = names.map { name =>
      if (name.isEmpty)
        none
      else
        name.some
    }.asRight
  }

  implicit object ParseableHeaderInstances extends MonadError[ParseableHeader, HeaderError] with SemigroupK[ParseableHeader] {
    def flatMap[A, B](fa: ParseableHeader[A])(f: A => ParseableHeader[B]): ParseableHeader[B] =
      names => fa(names).flatMap(f(_)(names))

    def handleErrorWith[A](fa: ParseableHeader[A])(f: HeaderError => ParseableHeader[A]): ParseableHeader[A] =
      names => fa(names).leftFlatMap(f(_)(names))

    def pure[A](x: A): ParseableHeader[A] =
      _ => Right(x)

    def raiseError[A](e: HeaderError): ParseableHeader[A] =
      _ => Left(e)

    def tailRecM[A, B](a: A)(f: A => ParseableHeader[Either[A, B]]): ParseableHeader[B] = {
      @tailrec
      def step(names: NonEmptyList[String], a: A): HeaderResult[B] =
        f(a)(names) match {
          case left @ Left(_)          => left.rightCast[B]
          case Right(Left(a))          => step(names, a)
          case Right(right @ Right(_)) => right.leftCast[HeaderError]
        }
      names => step(names, a)
    }

    override def combineK[A](x: ParseableHeader[A], y: ParseableHeader[A]): ParseableHeader[A] = names =>
      x(names) match {
        case Left(_)      => y(names)
        case r @ Right(_) => r
      }
  }

}
