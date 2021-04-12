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

import cats.implicits._

import shapeless._
import shapeless.labelled._

trait MapShapedCsvRowDecoder[Repr] extends CsvRowDecoder[Repr, String]

object MapShapedCsvRowDecoder extends LowPriorityMapShapedCsvRowDecoder1 {

  implicit def hnilRowDecoder[Wrapped]: WithDefaults[Wrapped, HNil, HNil, HNil, HNil] =
    (_: CsvRow[String], _: HNil, _: HNil, _: HNil) => Right(HNil)

  implicit def optionHconsRowDecoder[Wrapped,
                                     Key <: Symbol,
                                     Head,
                                     Tail <: HList,
                                     DefaultTail <: HList,
                                     Name,
                                     NamesTail <: HList,
                                     EmbedTail <: HList](implicit
      witness: Witness.Aux[Key],
      Head: CellDecoder[Head],
      ev: <:<[Name, Option[CsvName]],
      Tail: Lazy[WithDefaults[Wrapped, Tail, DefaultTail, NamesTail, EmbedTail]])
      : WithDefaults[Wrapped,
                     FieldType[Key, Option[Head]] :: Tail,
                     Option[Option[Head]] :: DefaultTail,
                     Name :: NamesTail,
                     None.type :: EmbedTail] =
    (row: CsvRow[String],
     default: Option[Option[Head]] :: DefaultTail,
     names: Name :: NamesTail,
     embeds: None.type :: EmbedTail) => {
      val head = row(names.head.fold(witness.value.name)(_.name)) match {
        case Some(head) if head.nonEmpty => Head(head).map(Some(_))
        case _                           => Right(default.head.flatten)
      }
      for {
        head <- head
        tail <- Tail.value.fromWithDefault(row, default.tail, names.tail, embeds.tail)
      } yield field[Key](head) :: tail
    }

  implicit def optionHconsEmbedRowDecoder[Wrapped,
                                          Key <: Symbol,
                                          Head,
                                          Tail <: HList,
                                          DefaultTail <: HList,
                                          NamesTail <: HList,
                                          EmbedTail <: HList](implicit
      witness: Witness.Aux[Key],
      Head: CsvRowDecoder[Option[Head], String],
      Tail: Lazy[WithDefaults[Wrapped, Tail, DefaultTail, NamesTail, EmbedTail]])
      : WithDefaults[Wrapped,
                     FieldType[Key, Option[Head]] :: Tail,
                     Option[Option[Head]] :: DefaultTail,
                     None.type :: NamesTail,
                     Some[CsvEmbed] :: EmbedTail] =
    (row: CsvRow[String],
     default: Option[Option[Head]] :: DefaultTail,
     names: None.type :: NamesTail,
     embeds: Some[CsvEmbed] :: EmbedTail) => {
      for {
        head <- (Head(row), default.head) match {
          case (r @ Right(_), _)                                    => r
          case (Left(_: DecoderError.ColumnMissing), Some(default)) => Right(default)
          case (Left(_: DecoderError.ColumnMissing), None)          => Right(None)
          case (l @ Left(_), _)                                     => l
        }
        tail <- Tail.value.fromWithDefault(row, default.tail, names.tail, embeds.tail)
      } yield field[Key](head) :: tail
    }

}

trait LowPriorityMapShapedCsvRowDecoder1 {

  trait WithDefaults[Wrapped, Repr, DefaultRepr, NameAnno, EmbedAnno] {
    def fromWithDefault(row: CsvRow[String],
                        default: DefaultRepr,
                        names: NameAnno,
                        embeds: EmbedAnno): DecoderResult[Repr]
  }

  implicit def hconsRowDecoder[Wrapped,
                               Key <: Symbol,
                               Head,
                               Tail <: HList,
                               DefaultTail <: HList,
                               Name,
                               NamesTail <: HList,
                               EmbedTail <: HList](implicit
      witness: Witness.Aux[Key],
      Head: CellDecoder[Head],
      ev: <:<[Name, Option[CsvName]],
      Tail: Lazy[WithDefaults[Wrapped, Tail, DefaultTail, NamesTail, EmbedTail]])
      : WithDefaults[Wrapped,
                     FieldType[Key, Head] :: Tail,
                     Option[Head] :: DefaultTail,
                     Name :: NamesTail,
                     None.type :: EmbedTail] =
    (row: CsvRow[String],
     default: Option[Head] :: DefaultTail,
     names: Name :: NamesTail,
     embeds: None.type :: EmbedTail) => {
      val head = row(names.head.fold(witness.value.name)(_.name)) match {
        case Some(head) if head.nonEmpty =>
          Head(head)
        case _ =>
          default.head.liftTo[DecoderResult](
            new DecoderError.ColumnMissing(s"unknown column name '${witness.value.name}'"))
      }
      for {
        head <- head
        tail <- Tail.value.fromWithDefault(row, default.tail, names.tail, embeds.tail)
      } yield field[Key](head) :: tail
    }

  implicit def hconsEmbedRowDecoder[Wrapped,
                                    Key <: Symbol,
                                    Head,
                                    Tail <: HList,
                                    DefaultTail <: HList,
                                    NamesTail <: HList,
                                    EmbedTail <: HList](implicit
      witness: Witness.Aux[Key],
      Head: CsvRowDecoder[Head, String],
      Tail: Lazy[WithDefaults[Wrapped, Tail, DefaultTail, NamesTail, EmbedTail]])
      : WithDefaults[Wrapped,
                     FieldType[Key, Head] :: Tail,
                     Option[Head] :: DefaultTail,
                     None.type :: NamesTail,
                     Some[CsvEmbed] :: EmbedTail] =
    (row: CsvRow[String],
     default: Option[Head] :: DefaultTail,
     names: None.type :: NamesTail,
     embeds: Some[CsvEmbed] :: EmbedTail) => {
      for {
        head <- (Head(row), default.head) match {
          case (r @ Right(_), _)                                    => r
          case (Left(_: DecoderError.ColumnMissing), Some(default)) => Right(default)
          case (l @ Left(_), _)                                     => l
        }
        tail <- Tail.value.fromWithDefault(row, default.tail, names.tail, embeds.tail)
      } yield field[Key](head) :: tail
    }

}
