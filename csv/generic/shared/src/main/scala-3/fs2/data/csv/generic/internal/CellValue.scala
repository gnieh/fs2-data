/*
 * Copyright 2024 fs2-data Project
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

package fs2.data.csv.generic.internal

import fs2.data.csv.generic.CsvValue
import shapeless3.deriving.Annotation

import scala.compiletime.*
import scala.deriving.*

trait CellValue[T] {
  def value: String
}

private class AnnotationCellValue[T](a: Annotation[CsvValue, T]) extends CellValue[T] {
  def value: String = a().value
}

private class ConstantCellValue[T](val value: String) extends CellValue[T]

object CellValue {
  inline given deriveSingleton[T](using m: Mirror.ProductOf[T] { type MirroredElemTypes = EmptyTuple }): CellValue[T] =
    summonFrom {
      case a: Annotation[CsvValue, T] => new AnnotationCellValue[T](a)
      case _                          => new ConstantCellValue[T](constValue[m.MirroredLabel])
    }
}
