/*
 * Copyright 2023 Lucas Satabin
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

package fs2.data.csv

import cats.syntax.all._

import scala.annotation.nowarn

@nowarn("msg=parameter.* L in method .+ is never used") trait LiteralCellEncoders {

  implicit final def literalStringEncoder[L <: String](implicit L: ValueOf[L]): CellEncoder[L] =
    CellEncoder.stringEncoder.narrow

  implicit final def literalCharEncoder[L <: Char](implicit L: ValueOf[L]): CellEncoder[L] =
    CellEncoder.charEncoder.narrow

  implicit final def literalByteEncoder[L <: Byte](implicit L: ValueOf[L]): CellEncoder[L] =
    CellEncoder.byteEncoder.narrow

  implicit final def literalShortEncoder[L <: Short](implicit L: ValueOf[L]): CellEncoder[L] =
    CellEncoder.shortEncoder.narrow

  implicit final def literalIntEncoder[L <: Int](implicit L: ValueOf[L]): CellEncoder[L] =
    CellEncoder.intEncoder.narrow

  implicit final def literalLongEncoder[L <: Long](implicit L: ValueOf[L]): CellEncoder[L] =
    CellEncoder.longEncoder.narrow

  implicit final def literalFloatEncoder[L <: Float](implicit L: ValueOf[L]): CellEncoder[L] =
    CellEncoder.floatEncoder.narrow

  implicit final def literalDoubleEncoder[L <: Double](implicit L: ValueOf[L]): CellEncoder[L] =
    CellEncoder.doubleEncoder.narrow

  implicit final def literalBooleanEncoder[L <: Boolean](implicit L: ValueOf[L]): CellEncoder[L] =
    CellEncoder.booleanEncoder.narrow

}
