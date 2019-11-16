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

import scala.util.Try

/** Describes how a cell can be decoded to the given type.
  */
trait CellDecoder[T] {
  def apply(cell: String): DecoderResult[T]
}

object CellDecoder {

  implicit object CellDecoderFunctor extends Functor[CellDecoder] {
    def map[A, B](fa: CellDecoder[A])(f: A => B): CellDecoder[B] =
      s => fa(s).map(f)
  }

  def apply[T: CellDecoder]: CellDecoder[T] = implicitly[CellDecoder[T]]

  implicit val shortDecoder: CellDecoder[Short] = s =>
    Try(s.toShort).toEither.leftMap(new DecoderError(s"unable to decode '$s' as a short", _))
  implicit val charDecoder: CellDecoder[Char] = s =>
    if (s.size == 1)
      Right(s(0))
    else
      Left(new DecoderError(s"unable to decode '$s' as a character"))
  implicit val intDecoder: CellDecoder[Int] = s =>
    Try(s.toInt).toEither.leftMap(new DecoderError(s"unable to decode '$s' as an integer", _))
  implicit val longDecoder: CellDecoder[Long] = s =>
    Try(s.toLong).toEither.leftMap(new DecoderError(s"unable to decode '$s' as a long", _))
  implicit val floatDecoder: CellDecoder[Float] = s =>
    Try(s.toFloat).toEither.leftMap(new DecoderError(s"unable to decode '$s' as a float", _))
  implicit val doubleDecoder: CellDecoder[Double] = s =>
    Try(s.toDouble).toEither.leftMap(new DecoderError(s"unable to decode '$s' as a double", _))
  implicit val stringDecoder: CellDecoder[String] = s => Right(s)

}
