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
package fs2.data.csv.generic.internal

import cats.data.NonEmptyList
import fs2.data.csv.{CellEncoder, CsvName, CsvRow, CsvRowEncoder}
import shapeless._
import shapeless.labelled._

private[generic] trait MapShapedCsvRowEncoder[Repr] extends CsvRowEncoder[Repr, String]

private[generic] object MapShapedCsvRowEncoder extends LowPrioMapShapedCsvRowEncoderImplicits {

  implicit def lastElemRowEncoder[Wrapped, Repr, Anno, Key <: Symbol](implicit
      Last: CellEncoder[Repr],
      ev: <:<[Anno, Option[CsvName]],
      witness: Witness.Aux[Key]): WithAnnotations[Wrapped, FieldType[Key, Repr] :: HNil, Anno :: HNil] =
    (row: Repr :: HNil, annotation: Anno :: HNil) =>
      CsvRow.unsafe(NonEmptyList.one(Last(row.head)),
                    NonEmptyList.one(annotation.head.fold(witness.value.name)(_.name)))

}

private[generic] trait LowPrioMapShapedCsvRowEncoderImplicits {
  trait WithAnnotations[Wrapped, Repr, AnnoRepr] {
    def fromWithAnnotation(row: Repr, annotation: AnnoRepr): CsvRow[String]
  }

  implicit def hconsRowEncoder[Wrapped,
                               Key <: Symbol,
                               Head,
                               Tail <: HList,
                               DefaultTail <: HList,
                               Anno,
                               AnnoTail <: HList](implicit
      witness: Witness.Aux[Key],
      Head: CellEncoder[Head],
      ev: <:<[Anno, Option[CsvName]],
      Tail: Lazy[WithAnnotations[Wrapped, Tail, AnnoTail]])
      : WithAnnotations[Wrapped, FieldType[Key, Head] :: Tail, Anno :: AnnoTail] =
    (row: FieldType[Key, Head] :: Tail, annotation: Anno :: AnnoTail) => {
      val tailRow = Tail.value.fromWithAnnotation(row.tail, annotation.tail)
      CsvRow.unsafe(NonEmptyList(Head(row.head), tailRow.values.toList),
                    NonEmptyList(annotation.head.fold(witness.value.name)(_.name), tailRow.headers.get.toList))
    }
}
