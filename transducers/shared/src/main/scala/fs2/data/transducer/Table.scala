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

package fs2.data.transducer

import scala.annotation.implicitNotFound

/** A typeclass indicating that some type `T` can be used as a lookup table.
  */
@implicitNotFound(
  "Cannot prove that type ${T} can be used as a lookup table. Make sure to provide an implicit instance of `fs2.data.transducer.Table[${T}]` in scope")
trait Table[T[_, _]] extends NTable[T] {
  def get[From, To](t: T[From, To])(from: From): Option[To]
  def getOrdered[From, To](t: T[From, To])(from: From): List[To] = get(t)(from).toList
}

object Table {

  implicit object PartialFunctionTable extends Table[PartialFunction] {
    def get[From, To](m: PartialFunction[From, To])(from: From): Option[To] =
      m.lift(from)
  }

  implicit object MapTable extends Table[Map] {
    def get[From, To](m: Map[From, To])(from: From): Option[To] = m.get(from)
  }
}

@implicitNotFound(
  "Cannot prove that type ${T} can be used as a non deterministic lookup table. Make sure to provide an implicit instance of `fs2.data.transducer.NTable[${T}]` in scope")
trait NTable[T[_, _]] {
  def getOrdered[From, To](t: T[From, To])(f: From): List[To]
}
