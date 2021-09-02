/*
 * Copyright 2021 Lucas Satabin
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

import fs2.data.csv.CellEncoder
import fs2.data.csv.generic.CsvValue
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.IsHCons

trait DerivedCellEncoder[T] extends CellEncoder[T]

object DerivedCellEncoder extends DerivedCellEncoderInstances0 {

  // Unary Products

  final implicit def unaryProductEncoder[A <: Product, L <: HList, H](implicit
      gen: Generic.Aux[A, L],
      ev: IsHCons.Aux[L, H, HNil],
      cc: CellEncoder[H]): DerivedCellEncoder[A] =
    a => cc.contramap[A](gen.to(_).head).apply(a)

  // Coproducts

  final implicit def coproductEncoder[T, Repr <: Coproduct](implicit
      gen: LabelledGeneric.Aux[T, Repr],
      cc: Lazy[DerivedCellEncoder[Repr]]): DerivedCellEncoder[T] =
    t => cc.value.contramap(gen.to)(t)

  final implicit val encodeCNil: DerivedCellEncoder[CNil] = (_: CNil) => sys.error("Can't happen")

  final implicit def encodeCCons[K <: Symbol, L, R <: Coproduct](implicit
      encodeL: CellEncoder[L],
      encodeR: Lazy[DerivedCellEncoder[R]]): DerivedCellEncoder[FieldType[K, L] :+: R] = {
    case Inl(head) => encodeL(head)
    case Inr(tail) => encodeR.value(tail)
  }

}

private[generic] trait DerivedCellEncoderInstances0 extends DerivedCellEncoderInstances1 {
  final implicit def encodeCConsObjAnnotated[K <: Symbol, L, R <: Coproduct](implicit
      annotation: Annotation[CsvValue, L],
      encodeR: Lazy[DerivedCellEncoder[R]]): DerivedCellEncoder[FieldType[K, L] :+: R] = {
    case Inl(_)    => annotation().value
    case Inr(tail) => encodeR.value(tail)
  }
}

private[generic] trait DerivedCellEncoderInstances1 {
  final implicit def encodeCConsObj[K <: Symbol, L, R <: Coproduct](implicit
      witK: Witness.Aux[K],
      encodeR: Lazy[DerivedCellEncoder[R]]): DerivedCellEncoder[FieldType[K, L] :+: R] = {
    case Inl(_)    => witK.value.name
    case Inr(tail) => encodeR.value(tail)
  }
}
