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
      def apply(cells: NonEmptyList[String]): DecoderResult[Option[Head] :: HNil] =
        cells match {
          case NonEmptyList(c, Nil) =>
            if(c.isEmpty)
              Right(None :: HNil)
            else
              Head(c).map(Some(_) :: HNil)
          case _ =>
            Left(new DecoderError(s"expected 1 element but got ${cells.size}"))
        }

    }

  implicit def hconsOptionDecoder[Head, Tail <: HList](implicit Head: CellDecoder[Head],
                                                 Tail: Lazy[SeqShapedRowDecoder[Tail]]): SeqShapedRowDecoder[Option[Head] :: Tail] =
    new SeqShapedRowDecoder[Option[Head] :: Tail] {
      def apply(cells: NonEmptyList[String]): DecoderResult[Option[Head] :: Tail] =
        for {
          tail <- NonEmptyList.fromList(cells.tail).liftTo[DecoderResult](new DecoderError("unexpect end of row"))
          head <- if(cells.head.isEmpty) Right(None) else Head(cells.head).map(Some(_))
          tail <- Tail.value(tail)
        } yield head :: tail
    }

}

trait LowPrioritySeqShapedRowDecoder1 {

  implicit def hnilDecoder[Head](implicit Head: CellDecoder[Head]): SeqShapedRowDecoder[Head :: HNil] =
    new SeqShapedRowDecoder[Head :: HNil] {
      def apply(cells: NonEmptyList[String]): DecoderResult[Head :: HNil] =
        cells match {
          case NonEmptyList(c, Nil) =>
            Head(c).map(_ :: HNil)
          case _ =>
            Left(new DecoderError(s"expected 1 element but got ${cells.size}"))
        }

    }

  implicit def hconsDecoder[Head, Tail <: HList](implicit Head: CellDecoder[Head],
                                                 Tail: Lazy[SeqShapedRowDecoder[Tail]]): SeqShapedRowDecoder[Head :: Tail] =
    new SeqShapedRowDecoder[Head :: Tail] {
      def apply(cells: NonEmptyList[String]): DecoderResult[Head :: Tail] =
        for {
          tail <- NonEmptyList.fromList(cells.tail).liftTo[DecoderResult](new DecoderError("unexpect end of row"))
          head <- Head(cells.head)
          tail <- Tail.value(tail)
        } yield head :: tail
    }

}
