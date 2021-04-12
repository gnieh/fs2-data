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
package fs2.data.csv.generic

import cats.data.NonEmptyList
import fs2.data.csv.{CellEncoder, CsvRow, CsvRowEncoder}
import shapeless._
import shapeless.labelled._

trait MapShapedCsvRowEncoder[Repr] extends CsvRowEncoder[Repr, String]

object MapShapedCsvRowEncoder extends LowPrioMapShapedCsvRowEncoderImplicits {

  implicit def lastElemRowEncoder[Wrapped, Repr, NameAnno, Key <: Symbol](implicit
      Last: CellEncoder[Repr],
      ev: <:<[NameAnno, Option[CsvName]],
      witness: Witness.Aux[Key])
      : WithAnnotations[Wrapped, FieldType[Key, Repr] :: HNil, NameAnno :: HNil, None.type :: HNil] =
    (row: Repr :: HNil, names: NameAnno :: HNil, _: None.type :: HNil) =>
      CsvRow.unsafe(NonEmptyList.one(Last(row.head)), NonEmptyList.one(names.head.fold(witness.value.name)(_.name)))

  implicit def lastElemEmbedRowEncoder[Wrapped, Repr, NameAnno, Key <: Symbol](implicit
      Last: CsvRowEncoder[Repr, String],
      names: <:<[NameAnno, None.type], // renaming is mutually exclusive with embedding
      witness: Witness.Aux[Key])
      : WithAnnotations[Wrapped, FieldType[Key, Repr] :: HNil, NameAnno :: HNil, Some[CsvEmbed] :: HNil] =
    (row: Repr :: HNil, _: NameAnno :: HNil, _: Some[CsvEmbed] :: HNil) => Last(row.head)

}

trait LowPrioMapShapedCsvRowEncoderImplicits {
  trait WithAnnotations[Wrapped, Repr, NameAnnoRepr, EmbedAnnoRepr] {
    def fromWithAnnotation(row: Repr, names: NameAnnoRepr, embeds: EmbedAnnoRepr): CsvRow[String]
  }

  implicit def hconsRowEncoder[Wrapped,
                               Key <: Symbol,
                               Head,
                               Tail <: HList,
                               DefaultTail <: HList,
                               NameAnno,
                               NameAnnoTail <: HList,
                               EmbedAnnoTail <: HList](implicit
      witness: Witness.Aux[Key],
      Head: CellEncoder[Head],
      ev: <:<[NameAnno, Option[CsvName]],
      Tail: Lazy[WithAnnotations[Wrapped, Tail, NameAnnoTail, EmbedAnnoTail]])
      : WithAnnotations[Wrapped, FieldType[Key, Head] :: Tail, NameAnno :: NameAnnoTail, None.type :: EmbedAnnoTail] =
    (row: FieldType[Key, Head] :: Tail, names: NameAnno :: NameAnnoTail, embeds: None.type :: EmbedAnnoTail) => {
      val tailRow = Tail.value.fromWithAnnotation(row.tail, names.tail, embeds.tail)
      CsvRow.unsafe(NonEmptyList(Head(row.head), tailRow.values.toList),
                    NonEmptyList(names.head.fold(witness.value.name)(_.name), tailRow.headers.get.toList))
    }

  implicit def hconsEmbedRowEncoder[Wrapped,
                                    Key <: Symbol,
                                    Head,
                                    Tail <: HList,
                                    DefaultTail <: HList,
                                    NameAnnoTail <: HList,
                                    EmbedAnnoTail <: HList](implicit
      witness: Witness.Aux[Key],
      Head: CsvRowEncoder[Head, String],
      names: <:<[None.type, Option[CsvName]], // renaming is mutually exclusive with embedding
      Tail: Lazy[WithAnnotations[Wrapped, Tail, NameAnnoTail, EmbedAnnoTail]])
      : WithAnnotations[Wrapped,
                        FieldType[Key, Head] :: Tail,
                        None.type :: NameAnnoTail,
                        Some[CsvEmbed] :: EmbedAnnoTail] =
    (row: FieldType[Key, Head] :: Tail, names: None.type :: NameAnnoTail, embeds: Some[CsvEmbed] :: EmbedAnnoTail) =>
      Head(row.head) ::: Tail.value.fromWithAnnotation(row.tail, names.tail, embeds.tail)
}
