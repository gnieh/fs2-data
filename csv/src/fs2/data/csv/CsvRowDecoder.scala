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

/** Describes how a row can be decoded to the given type.
  */
trait CsvRowDecoder[T, Header] {
  def apply(row: CsvRow[Header]): DecoderResult[T]
}

object CsvRowDecoder {

  implicit def CsvRowDecoderFunctor[Header]: Functor[CsvRowDecoder[*, Header]] =
    new Functor[CsvRowDecoder[*, Header]] {
      def map[A, B](fa: CsvRowDecoder[A, Header])(f: A => B): CsvRowDecoder[B, Header] =
        row => fa(row).map(f)
    }

  implicit def RowDecoderCsvRowDecoder[T](implicit T: RowDecoder[T]): CsvRowDecoder[T, Nothing] =
    new CsvRowDecoder[T, Nothing] {
      def apply(row: CsvRow[Nothing]): DecoderResult[T] =
        T(row.values)
    }

  def apply[Header, T: CsvRowDecoder[*, Header]]: CsvRowDecoder[T, Header] = implicitly[CsvRowDecoder[T, Header]]

}
