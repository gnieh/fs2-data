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
import cats.data.NonEmptyList

/** Describes how a row can be encoded from a value of the given type.
  */
trait RowEncoder[T] {
  def apply(elem: T): NonEmptyList[String]

  def contramap[B](f: B => T): RowEncoder[B] = elem => apply(f(elem))
}

object RowEncoder extends ExportedRowEncoders {

  implicit object RowEncoderContravariant extends Contravariant[RowEncoder] {
    override def contramap[A, B](fa: RowEncoder[A])(f: B => A): RowEncoder[B] =
      fa.contramap(f)
  }

  def apply[T: RowEncoder]: RowEncoder[T] = implicitly[RowEncoder[T]]

}

trait ExportedRowEncoders {
  implicit def exportedRowEncoders[A](implicit exported: Exported[RowEncoder[A]]): RowEncoder[A] =
    exported.instance
}
