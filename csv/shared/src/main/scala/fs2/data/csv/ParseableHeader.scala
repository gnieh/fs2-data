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

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.{Functor, SemigroupK}

/** A type class describing what it means to be a parseable
  * CSV header.
  */
trait ParseableHeader[Header] {

  def apply(names: NonEmptyList[String]): HeaderResult[Header]

}

object ParseableHeader {

  def apply[Header: ParseableHeader]: ParseableHeader[Header] =
    implicitly[ParseableHeader[Header]]

  /**
    * Define an instance of [[ParseableHeader]] that decodes each column individually using the provided
    * `parse` function.
    */
  def instance[Header](parse: String => Either[HeaderError, Header]): ParseableHeader[Header] =
    _.traverse[Either[HeaderError, *], Header](parse)

  implicit object NothingParseableHeader extends ParseableHeader[Nothing] {
    def apply(names: NonEmptyList[String]): HeaderResult[Nothing] =
      new HeaderError("no headers are expected").asLeft[NonEmptyList[Nothing]]
  }

  implicit object StringParseableHeader extends ParseableHeader[String] {
    def apply(names: NonEmptyList[String]): HeaderResult[String] = names.asRight
  }

  implicit object NonEmptyStringParseableHeader extends ParseableHeader[Option[String]] {
    def apply(names: NonEmptyList[String]): HeaderResult[Option[String]] =
      names.map { name =>
        if (name.isEmpty)
          None
        else
          Some(name)
      }.asRight
  }

  implicit object ParseableHeaderInstances extends Functor[ParseableHeader] with SemigroupK[ParseableHeader] {
    override def map[A, B](fa: ParseableHeader[A])(f: A => B): ParseableHeader[B] = names => fa(names).map(_.map(f))

    override def combineK[A](x: ParseableHeader[A], y: ParseableHeader[A]): ParseableHeader[A] =
      names =>
        x(names) match {
          case Left(_)      => y(names)
          case r @ Right(_) => r
        }
  }

  def liftCellDecoder[T](implicit cellDecoder: CellDecoder[T]): ParseableHeader[T] =
    _.traverse[Either[HeaderError, *], T](cellDecoder(_).leftMap(e => new HeaderError(e.getMessage)))

}
