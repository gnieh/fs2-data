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

trait LiteralCellDecoders {

  private[this] abstract class LiteralCellDecoder[A, L <: A](decodeA: CellDecoder[A], L: ValueOf[L])
      extends CellDecoder[L] {
    protected[this] def check(a: A): Boolean
    protected[this] def message: String

    final def apply(s: String): DecoderResult[L] = decodeA(s) match {
      case r @ Right(value) if check(value) => r.asInstanceOf[DecoderResult[L]]
      case _                                => Left(new DecoderError(s"Unable to decode literal $message"))
    }
  }

  implicit final def literalStringDecoder[L <: String](implicit L: ValueOf[L]): CellDecoder[L] =
    new LiteralCellDecoder[String, L](CellDecoder.stringDecoder, L) {
      protected[this] final def check(a: String): Boolean = a == L.value
      protected[this] final def message: String = s"""String("${L.value}")"""
    }

  implicit final def literalCharDecoder[L <: Char](implicit L: ValueOf[L]): CellDecoder[L] =
    new LiteralCellDecoder[Char, L](CellDecoder.charDecoder, L) {
      protected[this] final def check(a: Char): Boolean = a == L.value
      protected[this] final def message: String = s"""Char("${L.value}")"""
    }

  implicit final def literalByteDecoder[L <: Byte](implicit L: ValueOf[L]): CellDecoder[L] =
    new LiteralCellDecoder[Byte, L](CellDecoder.byteDecoder, L) {
      protected[this] final def check(a: Byte): Boolean = a == L.value
      protected[this] final def message: String = s"""Byte("${L.value}")"""
    }

  implicit final def literalShortDecoder[L <: Short](implicit L: ValueOf[L]): CellDecoder[L] =
    new LiteralCellDecoder[Short, L](CellDecoder.shortDecoder, L) {
      protected[this] final def check(a: Short): Boolean = a == L.value
      protected[this] final def message: String = s"""Short("${L.value}")"""
    }

  implicit final def literalIntDecoder[L <: Int](implicit L: ValueOf[L]): CellDecoder[L] =
    new LiteralCellDecoder[Int, L](CellDecoder.intDecoder, L) {
      protected[this] final def check(a: Int): Boolean = a == L.value
      protected[this] final def message: String = s"""Int("${L.value}")"""
    }

  implicit final def literalLongDecoder[L <: Long](implicit L: ValueOf[L]): CellDecoder[L] =
    new LiteralCellDecoder[Long, L](CellDecoder.longDecoder, L) {
      protected[this] final def check(a: Long): Boolean = a == L.value
      protected[this] final def message: String = s"""Long("${L.value}")"""
    }

  implicit final def literalFloatDecoder[L <: Float](implicit L: ValueOf[L]): CellDecoder[L] =
    new LiteralCellDecoder[Float, L](CellDecoder.floatDecoder, L) {
      protected[this] final def check(a: Float): Boolean = java.lang.Float.compare(L.value, a) == 0
      protected[this] final def message: String = s"""Float("${L.value}")"""
    }

  implicit final def literalDoubleDecoder[L <: Double](implicit L: ValueOf[L]): CellDecoder[L] =
    new LiteralCellDecoder[Double, L](CellDecoder.doubleDecoder, L) {
      protected[this] final def check(a: Double): Boolean = java.lang.Double.compare(L.value, a) == 0
      protected[this] final def message: String = s"""Double("${L.value}")"""
    }

  implicit final def literalBooleanDecoder[L <: Boolean](implicit L: ValueOf[L]): CellDecoder[L] =
    new LiteralCellDecoder[Boolean, L](CellDecoder.booleanDecoder, L) {
      protected[this] final def check(a: Boolean): Boolean = a == L.value
      protected[this] final def message: String = s"""Boolean("${L.value}")"""
    }

}
