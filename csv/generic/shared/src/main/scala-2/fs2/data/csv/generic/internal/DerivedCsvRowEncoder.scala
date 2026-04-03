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

import cats.data.NonEmptyList
import fs2.data.csv.{CsvRow, CsvRowEncoder, StaticHeaders}
import fs2.data.csv.generic.CsvName
import shapeless._

trait DerivedCsvRowEncoder[T] extends CsvRowEncoder[T, String] with StaticHeaders[T, String]

object DerivedCsvRowEncoder {

  final implicit def productWriter[T, Repr <: HList, AnnoRepr <: HList](implicit
      gen: LabelledGeneric.Aux[T, Repr],
      annotations: Annotations.Aux[CsvName, T, AnnoRepr],
      cc: Lazy[MapShapedCsvRowEncoder.WithAnnotations[Repr, AnnoRepr]]): DerivedCsvRowEncoder[T] = {
    val annos = annotations()
    new DerivedCsvRowEncoder[T] {
      override val headers: NonEmptyList[String] = NonEmptyList.fromListUnsafe(cc.value.headers(annos))
      override def apply(elem: T): CsvRow[String] =
        CsvRow.unsafe(NonEmptyList.fromListUnsafe(cc.value.rawRow(gen.to(elem))), headers)
    }
  }

}
