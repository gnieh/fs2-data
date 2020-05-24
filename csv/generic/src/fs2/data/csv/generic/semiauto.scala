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
package fs2
package data
package csv
package generic

import shapeless._

object semiauto {

  def deriveRowDecoder[T](
      implicit T: Lazy[DerivedRowDecoder[T]]): RowDecoder[T] =
    T.value

  def deriveRowEncoder[T](
      implicit T: Lazy[DerivedRowEncoder[T]]): RowEncoder[T] =
    T.value

  def deriveCsvRowDecoder[T](
      implicit T: Lazy[DerivedCsvRowDecoder[T]]): CsvRowDecoder[T, String] =
    T.value

  def deriveCsvRowEncoder[T](
      implicit T: Lazy[DerivedCsvRowEncoder[T]]): CsvRowEncoder[T, String] =
    T.value

  def deriveCellDecoder[T](
      implicit T: Lazy[DerivedCellDecoder[T]]): CellDecoder[T] =
    T.value

  def deriveCellEncoder[T](
      implicit T: Lazy[DerivedCellEncoder[T]]): CellEncoder[T] =
    T.value

}
