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

import cats.implicits._
import fs2.data.csv._
import fs2.data.csv.generic.CsvName
import shapeless._
import shapeless.labelled._

trait MapShapedCsvRowDecoder[Repr] extends CsvRowDecoder[Repr, String]

object MapShapedCsvRowDecoder extends LowPriorityMapShapedCsvRowDecoder1 {

  implicit def hnilRowDecoder[Wrapped]: WithDefaults[Wrapped, HNil, HNil, HNil] =
    new WithDefaults[Wrapped, HNil, HNil, HNil] {
      def fromWithDefault(row: CsvRow[String], default: HNil, annotation: HNil): DecoderResult[HNil] =
        Right(HNil)
    }

  implicit def optionHconsRowDecoder[Wrapped,
                                     Key <: Symbol,
                                     Head,
                                     Tail <: HList,
                                     DefaultTail <: HList,
                                     Anno,
                                     AnnoTail <: HList](implicit
      witness: Witness.Aux[Key],
      Head: CellDecoder[Head],
      ev: <:<[Anno, Option[CsvName]],
      Tail: Lazy[WithDefaults[Wrapped, Tail, DefaultTail, AnnoTail]])
      : WithDefaults[Wrapped,
                     FieldType[Key, Option[Head]] :: Tail,
                     Option[Option[Head]] :: DefaultTail,
                     Anno :: AnnoTail] =
    new WithDefaults[Wrapped,
                     FieldType[Key, Option[Head]] :: Tail,
                     Option[Option[Head]] :: DefaultTail,
                     Anno :: AnnoTail] {
      def fromWithDefault(row: CsvRow[String],
                          default: Option[Option[Head]] :: DefaultTail,
                          anno: Anno :: AnnoTail): DecoderResult[FieldType[Key, Option[Head]] :: Tail] = {
        val head = row(anno.head.fold(witness.value.name)(_.name)) match {
          case Some(head) if head.nonEmpty => Head(head).bimap(_.withLine(row.line), Some(_))
          case _                           => Right(default.head.flatten)
        }
        for {
          head <- head
          tail <- Tail.value.fromWithDefault(row, default.tail, anno.tail)
        } yield field[Key](head) :: tail
      }
    }

}

private[generic] trait LowPriorityMapShapedCsvRowDecoder1 {

  trait WithDefaults[Wrapped, Repr, DefaultRepr, AnnoRepr] {
    def fromWithDefault(row: CsvRow[String], default: DefaultRepr, annotation: AnnoRepr): DecoderResult[Repr]
  }

  implicit def hconsRowDecoder[Wrapped,
                               Key <: Symbol,
                               Head,
                               Tail <: HList,
                               DefaultTail <: HList,
                               Anno,
                               AnnoTail <: HList](implicit
      witness: Witness.Aux[Key],
      Head: CellDecoder[Head],
      ev: <:<[Anno, Option[CsvName]],
      Tail: Lazy[WithDefaults[Wrapped, Tail, DefaultTail, AnnoTail]])
      : WithDefaults[Wrapped, FieldType[Key, Head] :: Tail, Option[Head] :: DefaultTail, Anno :: AnnoTail] =
    new WithDefaults[Wrapped, FieldType[Key, Head] :: Tail, Option[Head] :: DefaultTail, Anno :: AnnoTail] {
      def fromWithDefault(row: CsvRow[String],
                          default: Option[Head] :: DefaultTail,
                          anno: Anno :: AnnoTail): DecoderResult[FieldType[Key, Head] :: Tail] = {
        val head = row(anno.head.fold(witness.value.name)(_.name)) match {
          case Some(head) if head.nonEmpty =>
            Head(head).leftMap(_.withLine(row.line))
          case _ =>
            default.head.liftTo[DecoderResult](
              new DecoderError(s"unknown column name '${witness.value.name}'", row.line))
        }
        for {
          head <- head
          tail <- Tail.value.fromWithDefault(row, default.tail, anno.tail)
        } yield field[Key](head) :: tail
      }
    }

}
