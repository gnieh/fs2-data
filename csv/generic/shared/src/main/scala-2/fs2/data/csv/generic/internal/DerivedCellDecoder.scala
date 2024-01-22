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

import cats.syntax.all._
import fs2.data.csv.generic.CsvValue
import fs2.data.csv.{CellDecoder, DecoderError}
import shapeless._
import shapeless.labelled._

trait DerivedCellDecoder[T] extends CellDecoder[T]

object DerivedCellDecoder extends DerivedCellDecoderInstances0 with DerivedCellDecoderBinCompat {

  // Unary Products

  final implicit def unaryProductDecoder[A <: Product, L <: HList, H](implicit
      gen: Generic.Aux[A, L],
      ev: =:=[H :: HNil, L],
      cc: CellDecoder[H]): DerivedCellDecoder[A] =
    s => cc(s).map(v => gen.from(v :: HNil))

  // Coproducts

  final implicit def coproductDecoder[T, Repr <: Coproduct](implicit
      gen: LabelledGeneric.Aux[T, Repr],
      cc: Lazy[DerivedCellDecoder[Repr]]): DerivedCellDecoder[T] =
    s => cc.value(s).map(gen.from(_))

  final implicit val decodeCNil: DerivedCellDecoder[CNil] = (_: String) => Left(new DecoderError("CNil"))

  final implicit def decodeCCons[K <: Symbol, L, R <: Coproduct](implicit
      decodeL: CellDecoder[L],
      decodeR: Lazy[DerivedCellDecoder[R]]): DerivedCellDecoder[FieldType[K, L] :+: R] =
    s =>
      decodeL(s)
        .map[FieldType[K, L] :+: R](v => Inl(field[K](v)))
        .recoverWith { case _ => decodeR.value(s).map(Inr(_)) }

}

private[generic] trait DerivedCellDecoderInstances0 extends DerivedCellDecoderInstances1 {
  final implicit def decodeCConsObjAnnotated[K <: Symbol, L, R <: Coproduct](implicit
      witL: Witness.Aux[L],
      annotation: Annotation[CsvValue, L],
      decodeR: Lazy[DerivedCellDecoder[R]]): DerivedCellDecoder[FieldType[K, L] :+: R] =
    s =>
      if (annotation().value == s) Inl(field[K](witL.value)).asRight
      else decodeR.value(s).map(Inr(_))
}

private[generic] trait DerivedCellDecoderInstances1 {
  final implicit def decodeCConsObj[K <: Symbol, L, R <: Coproduct](implicit
      witK: Witness.Aux[K],
      witL: Witness.Aux[L],
      decodeR: Lazy[DerivedCellDecoder[R]]): DerivedCellDecoder[FieldType[K, L] :+: R] =
    s =>
      if (witK.value.name == s) Inl(field[K](witL.value)).asRight
      else decodeR.value(s).map(Inr(_))
}

// stubs for bincompat
trait DerivedCellDecoderBinCompat { self: DerivedCellDecoder.type =>
  import scala.annotation.unused

  private[internal] final def decodeCCons[K <: Symbol, L, R <: Coproduct](
      @unused witK: Witness.Aux[K],
      decodeL: CellDecoder[L],
      decodeR: Lazy[DerivedCellDecoder[R]]): DerivedCellDecoder[FieldType[K, L] :+: R] =
    self.decodeCCons(decodeL, decodeR)

  private[internal] final def decodeCConsObjAnnotated[K <: Symbol, L, R <: Coproduct](
      @unused witK: Witness.Aux[K],
      witL: Witness.Aux[L],
      annotation: Annotation[CsvValue, L],
      @unused gen: Generic.Aux[L, HNil],
      decodeR: Lazy[DerivedCellDecoder[R]]): DerivedCellDecoder[FieldType[K, L] :+: R] =
    self.decodeCConsObjAnnotated(witL, annotation, decodeR)

  private[internal] final def decodeCConsObj[K <: Symbol, L, R <: Coproduct](
      witK: Witness.Aux[K],
      witL: Witness.Aux[L],
      @unused gen: Generic.Aux[L, HNil],
      decodeR: Lazy[DerivedCellDecoder[R]]): DerivedCellDecoder[FieldType[K, L] :+: R] =
    self.decodeCConsObj(witK, witL, decodeR)
}
