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
package fs2.data.csv

import cats._

import scala.annotation.implicitNotFound

/** Describes how a row can be encoded from a value of the given type.
  */
@implicitNotFound(
  "No implicit RowEncoderF[H,  found for type ${T}.\nYou can define one using RowEncoderF[H, .instance, by calling contramap on another RowEncoderF[H,  or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val csvRowEncoder: RowEncoderF[H, [${T}] = deriveRowEncoderF[H, \nMake sure to have instances of CellEncoder for every member type in scope.\n")
trait RowEncoderF[H[a] <: Option[a], T, Header] {
  def apply(elem: T): RowF[H, Header]

  def contramap[B](f: B => T): RowEncoderF[H, B, Header] = elem => apply(f(elem))
}

object RowEncoderF extends ExportedRowEncoderF {

  implicit def RowEncoderF[H[a] <: Option[a], Header]: Contravariant[RowEncoderF[H, *, Header]] =
    new Contravariant[RowEncoderF[H, *, Header]] {
      override def contramap[A, B](fa: RowEncoderF[H, A, Header])(f: B => A): RowEncoderF[H, B, Header] =
        fa.contramap(f)
    }

  @inline
  def apply[T: RowEncoderF[H, *, Header], Header]: RowEncoderF[H, T, Header] = implicitly[RowEncoderF[H, T, Header]]

  @inline
  def instance[T, Header](f: T => CsvRow[Header]): RowEncoderF[H, T, Header] = f(_)
}

trait ExportedRowEncoderF {
  implicit def exportedRowEncoderF[H[a] <: Option[a], A](implicit
      exported: Exported[RowEncoderF[H, A, String]]): RowEncoderF[H, A, String] = exported.instance
}
