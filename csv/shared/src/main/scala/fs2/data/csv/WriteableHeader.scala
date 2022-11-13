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
import cats.data.NonEmptyList

trait WriteableHeader[Header] {
  def apply(headers: NonEmptyList[Header]): NonEmptyList[String]

  def contramap[B](f: B => Header): WriteableHeader[B] = headers => apply(headers.map(f))
}

object WriteableHeader {
  def apply[Header: WriteableHeader]: WriteableHeader[Header] =
    implicitly[WriteableHeader[Header]]

  def instance[Header](encode: Header => String): WriteableHeader[Header] =
    _.map(encode)

  implicit object StringWriteableHeader extends WriteableHeader[String] {
    def apply(names: NonEmptyList[String]): NonEmptyList[String] = names
  }

  implicit object WriteableHeaderInstances extends Contravariant[WriteableHeader] {
    override def contramap[A, B](fa: WriteableHeader[A])(f: B => A): WriteableHeader[B] = fa.contramap(f)
  }

  def liftCellEncoder[T](implicit cellEncoder: CellEncoder[T]): WriteableHeader[T] =
    _.map(cellEncoder(_))
}
