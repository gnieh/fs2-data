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

import cats.data.NonEmptyList
import cats.implicits._

import shapeless._

trait SeqShapedRowDecoder[Repr] extends RowDecoder[Repr]

object SeqShapedRowDecoder extends LowPrioritySeqShapedRowDecoder1 {

  implicit def hnilOptionDecoder[Head](implicit Head: CellDecoder[Head]): SeqShapedRowDecoder[Option[Head] :: HNil] =
    new SeqShapedRowDecoder[Option[Head] :: HNil] {
      def apply(row: Row): DecoderResult[Option[Head] :: HNil] =
        row.values match {
          case NonEmptyList(c, Nil) =>
            if (c.isEmpty)
              Right(None :: HNil)
            else
              Head(c).bimap(_.withLine(row.line), Some(_) :: HNil)
          case _ =>
            Left(new DecoderError(s"expected 1 element but got ${row.values.size}", row.line))
        }

    }

  implicit def hconsOptionDecoder[Head, Tail <: HList](implicit
      Head: CellDecoder[Head],
      Tail: Lazy[SeqShapedRowDecoder[Tail]]): SeqShapedRowDecoder[Option[Head] :: Tail] =
    new SeqShapedRowDecoder[Option[Head] :: Tail] {
      def apply(row: Row): DecoderResult[Option[Head] :: Tail] =
        for {
          tail <- NonEmptyList
            .fromList(row.values.tail)
            .liftTo[DecoderResult](new DecoderError("unexpected end of row", row.line))
          head <-
            if (row.values.head.isEmpty) Right(None) else Head(row.values.head).bimap(_.withLine(row.line), Some(_))
          tail <- Tail.value(Row(tail))
        } yield head :: tail
    }

}

trait LowPrioritySeqShapedRowDecoder1 {

  implicit def hnilDecoder[Head](implicit Head: CellDecoder[Head]): SeqShapedRowDecoder[Head :: HNil] =
    new SeqShapedRowDecoder[Head :: HNil] {
      def apply(row: Row): DecoderResult[Head :: HNil] =
        row.values match {
          case NonEmptyList(c, Nil) =>
            Head(c).bimap(_.withLine(row.line), _ :: HNil)
          case _ =>
            Left(new DecoderError(s"expected 1 element but got ${row.values.size}", row.line))
        }

    }

  implicit def hconsDecoder[Head, Tail <: HList](implicit
      Head: CellDecoder[Head],
      Tail: Lazy[SeqShapedRowDecoder[Tail]]): SeqShapedRowDecoder[Head :: Tail] =
    new SeqShapedRowDecoder[Head :: Tail] {
      def apply(row: Row): DecoderResult[Head :: Tail] =
        for {
          tail <- NonEmptyList
            .fromList(row.values.tail)
            .liftTo[DecoderResult](new DecoderError("unexpect end of row", row.line))
          head <- Head(row.values.head).leftMap(_.withLine(row.line))
          tail <- Tail.value(Row(tail))
        } yield head :: tail
    }

}
