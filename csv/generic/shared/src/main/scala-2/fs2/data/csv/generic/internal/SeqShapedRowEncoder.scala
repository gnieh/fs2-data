/*
 * Copyright 2019-2022 Lucas Satabin
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
import fs2.data.csv.{CellEncoder, Row, RowEncoder}
import shapeless._

trait SeqShapedRowEncoder[Repr] extends RowEncoder[Repr]

object SeqShapedRowEncoder {

  implicit def lastElemEncoder[Head](implicit Head: CellEncoder[Head]): SeqShapedRowEncoder[Head :: HNil] =
    (last: Head :: HNil) => Row(NonEmptyList.one(Head(last.head)))

  implicit def hconsEncoder[Head, Tail <: HList](implicit
      Head: CellEncoder[Head],
      Tail: Lazy[SeqShapedRowEncoder[Tail]]): SeqShapedRowEncoder[Head :: Tail] =
    (fields: Head :: Tail) => Row(NonEmptyList(Head(fields.head), Tail.value(fields.tail).values.toList))

}
