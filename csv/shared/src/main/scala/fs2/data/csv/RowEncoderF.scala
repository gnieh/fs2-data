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

package fs2.data.csv

import cats._
import cats.data.{NonEmptyList, NonEmptyMap}

import scala.annotation.{implicitNotFound, unused}

/** Describes how a row can be encoded from a value of the given type.
  */
@implicitNotFound(
  "No implicit RowEncoderF[H,  found for type ${T}.\nYou can define one using RowEncoderF[H, .instance, by calling contramap on another RowEncoderF[H,  or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val csvRowEncoder: RowEncoderF[H, [${T}] = deriveRowEncoderF[H, \nMake sure to have instances of CellEncoder for every member type in scope.\n")
@FunctionalInterface trait RowEncoderF[H[+a] <: Option[a], T, Header] {
  def apply(elem: T): RowF[H, Header]

  def contramap[B](f: B => T): RowEncoderF[H, B, Header] = elem => apply(f(elem))
}

object RowEncoderF extends ExportedRowEncoders {

  implicit def identityRowEncoderF[H[+a] <: Option[a], Header]: RowEncoderF[H, RowF[H, Header], Header] = identity

  implicit def RowEncoderF[H[+a] <: Option[a], Header]: Contravariant[RowEncoderF[H, *, Header]] =
    new Contravariant[RowEncoderF[H, *, Header]] {
      override def contramap[A, B](fa: RowEncoderF[H, A, Header])(f: B => A): RowEncoderF[H, B, Header] =
        fa.contramap(f)
    }

  @inline
  def apply[H[+a] <: Option[a], T: RowEncoderF[H, *, Header], Header]: RowEncoderF[H, T, Header] =
    implicitly[RowEncoderF[H, T, Header]]

  @inline
  def instance[H[+a] <: Option[a], T, Header](f: T => RowF[H, Header]): RowEncoderF[H, T, Header] = f(_)

  implicit val fromNelRowEncoder: RowEncoder[NonEmptyList[String]] =
    RowEncoder.instance(r => r)
}

trait ExportedRowEncoders {
  implicit def exportedRowEncoder[A](implicit exported: Exported[RowEncoder[A]]): RowEncoder[A] = exported.instance

}
