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

import fs2.data.csv.generic.CsvName
import fs2.data.csv.{CellEncoder, CsvRowEncoder}
import shapeless._
import shapeless.labelled._

trait MapShapedCsvRowEncoder[Repr] extends CsvRowEncoder[Repr, String]

object MapShapedCsvRowEncoder extends LowPrioMapShapedCsvRowEncoderImplicits {

  implicit def lastElemRowEncoder[Repr, Anno, Key <: Symbol](implicit
      Last: CellEncoder[Repr],
      ev: <:<[Anno, Option[CsvName]],
      witness: Witness.Aux[Key]): WithAnnotations[FieldType[Key, Repr] :: HNil, Anno :: HNil] =
    new WithAnnotations[FieldType[Key, Repr] :: HNil, Anno :: HNil] {
      override def headers(annotation: Anno :: HNil): List[String] =
        annotation.head.fold(witness.value.name)(_.name) :: Nil
      override def rawRow(repr: FieldType[Key, Repr] :: HNil): List[String] =
        Last(repr.head) :: Nil
    }

}

private[generic] trait LowPrioMapShapedCsvRowEncoderImplicits {
  trait WithAnnotations[Repr, AnnoRepr] {
    def headers(annotation: AnnoRepr): List[String]
    def rawRow(repr: Repr): List[String]
  }

  implicit def hconsRowEncoder[Key <: Symbol, Head, Tail <: HList, DefaultTail <: HList, Anno, AnnoTail <: HList](
      implicit
      witness: Witness.Aux[Key],
      Head: CellEncoder[Head],
      ev: <:<[Anno, Option[CsvName]],
      Tail: Lazy[WithAnnotations[Tail, AnnoTail]]): WithAnnotations[FieldType[Key, Head] :: Tail, Anno :: AnnoTail] = {
    val tail = Tail.value
    new WithAnnotations[FieldType[Key, Head] :: Tail, Anno :: AnnoTail] {
      override def headers(annotation: Anno :: AnnoTail): List[String] =
        annotation.head.fold(witness.value.name)(_.name) :: tail.headers(annotation.tail)
      override def rawRow(repr: FieldType[Key, Head] :: Tail): List[String] =
        Head(repr.head) :: tail.rawRow(repr.tail)
    }
  }
}
