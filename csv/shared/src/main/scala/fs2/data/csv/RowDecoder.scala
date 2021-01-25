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
import cats.implicits._
import cats.data.NonEmptyList

import scala.annotation.{implicitNotFound, tailrec}

/** Describes how a row can be decoded to the given type.
  *
  * `RowDecoder` provides convenient methods such as `map`, `emap`, or `flatMap`
  * to build new decoders out of more basic one.
  *
  * Actually, `RowDecoder` has a [[https://typelevel.org/cats/api/cats/MonadError.html cats `MonadError`]]
  * instance. To get the full power of it, import `cats.implicits._`.
  */

object RowDecoder {
  @inline
  def apply[T: RowDecoder]: RowDecoder[T] = implicitly[RowDecoder[T]]

  @inline
  def instance[T](f: NonEmptyList[String] => DecoderResult[T]): RowDecoder[T] = row => f(row.values)
}
