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
import cats.syntax.all._

import scala.annotation.implicitNotFound

/** Describes how a row can be encoded from a value of the given type.
  */
@implicitNotFound(
  "No implicit RowEncoderF[ found for type ${T}.\nYou can define one using RowEncoderF[.instance, by calling contramap on another RowEncoderF[ or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val csvRowEncoder: RowEncoderF[[${T}] = deriveRowEncoderF[\nMake sure to have instances of CellEncoder for every member type in scope.\n")
@FunctionalInterface trait RowEncoder[T] {
  def apply(elem: T): Row

  def contramap[B](f: B => T): RowEncoder[B] = elem => apply(f(elem))
}

object RowEncoder extends ExportedRowEncoders {
  @inline
  def apply[T: RowEncoder]: RowEncoder[T] = implicitly[RowEncoder[T]]

  @inline
  def instance[T](f: T => List[String]): RowEncoder[T] = (t: T) => Row(f(t))

  implicit def identityRowEncoder: RowEncoder[Row] = identity

  implicit def RowEncoder: ContravariantMonoidal[RowEncoder] =
    new ContravariantMonoidal[RowEncoder] {
      override def product[A, B](fa: RowEncoder[A], fb: RowEncoder[B]): RowEncoder[(A, B)] =
        (fa, fb).tupled

      override def unit: RowEncoder[Unit] =
        _ => Row(Nil, None)

      override def contramap[A, B](fa: RowEncoder[A])(f: B => A): RowEncoder[B] =
        fa.contramap(f)
    }

  implicit val fromNelRowEncoder: RowEncoder[List[String]] =
    instance[List[String]](r => r)
}

trait ExportedRowEncoders {
  implicit def exportedRowEncoder[A](implicit exported: Exported[RowEncoder[A]]): RowEncoder[A] = exported.instance

}
