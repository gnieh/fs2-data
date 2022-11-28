/*
 * Copyright 2022 Lucas Satabin
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

import cats.Contravariant
import cats.data.{NonEmptyList, NonEmptyMap}

import scala.annotation.implicitNotFound

/** Describes how a row can be encoded from a value of the given type.
  */
@implicitNotFound(
  "No implicit RowEncoder found for type ${T}.\nYou can define one using RowEncoder.instance, by calling contramap on another RowEncoder or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val rowEncoder: RowEncoder[${T}] = deriveRowEncoder\nMake sure to have instances of CellEncoder for every member type in scope.\n")
@FunctionalInterface trait RowEncoder[T] {
  def apply(elem: T): Row

  def contramap[B](f: B => T): RowEncoder[B] = elem => apply(f(elem))
}

object RowEncoder extends ExportedRowEncoders {

  @inline
  def apply[T: RowEncoder]: RowEncoder[T] = implicitly[RowEncoder[T]]

  @inline
  def instance[T](f: T => NonEmptyList[String]): RowEncoder[T] = (t: T) => Row(f(t))

  implicit val identityRowEncoder: RowEncoder[Row] = identity

  implicit def RowEncoderInstances: Contravariant[RowEncoder] =
    new Contravariant[RowEncoder] {
      override def contramap[A, B](fa: RowEncoder[A])(f: B => A): RowEncoder[B] =
        fa.contramap(f)
    }

  implicit def fromNonEmptyMapCsvRowEncoder[Header]: CsvRowEncoder[NonEmptyMap[Header, String], Header] =
    CsvRowEncoder.instance(_.toNel)

  implicit val fromNelRowEncoder: RowEncoder[NonEmptyList[String]] =
    RowEncoder.instance(r => r)
}

trait ExportedRowEncoders {
  implicit def exportedRowEncoder[A](implicit exported: Exported[RowEncoder[A]]): RowEncoder[A] = exported.instance
}
