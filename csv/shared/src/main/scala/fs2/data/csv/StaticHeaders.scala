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

import cats.Functor
import cats.data.NonEmptyList

/** Type class for types that have statically known headers. This is useful for encoding/decoding CSV rows
 * where the headers are fixed and can be determined at compile time. */
trait StaticHeaders[T, H] {
  def headers: NonEmptyList[H]
}

object StaticHeaders {
  @inline
  def apply[T, H](implicit sh: StaticHeaders[T, H]): StaticHeaders[T, H] = sh

  @inline
  def instance[T, H](hs: NonEmptyList[H]): StaticHeaders[T, H] =
    new StaticHeaders[T, H] {
      override def headers: NonEmptyList[H] = hs
    }

  implicit def headerFunctor[T]: Functor[StaticHeaders[T, *]] = new Functor[StaticHeaders[T, *]] {
    override def map[A, B](fa: StaticHeaders[T, A])(f: A => B): StaticHeaders[T, B] = instance(fa.headers.map(f))
  }

  implicit def forWriteableHeader[T, H](implicit
      W: WriteableHeader[H],
      H: StaticHeaders[T, H]): StaticHeaders[T, String] =
    instance(WriteableHeader[H].apply(H.headers))
}
