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

package fs2.data.matching

/** Represents a lookup table. This is useful to implement
  * finite state automata or transducers.
  */
trait Table[T, In, Out] {
  def get(table: T)(in: In): Option[Out]
}

object Table extends LowPriorityImplicits {
  def apply[T, I, O](implicit ev: Table[T, I, O]): Table[T, I, O] =
    ev

  implicit def MapTable[K, V]: Table[Map[K, V], K, V] = new Table[Map[K, V], K, V] {

    override def get(table: Map[K, V])(in: K): Option[V] = table.get(in)

  }

  implicit class TableOps[T](val t: T) extends AnyVal {
    def get[K, V](in: K)(implicit T: Table[T, K, V]): Option[V] =
      T.get(t)(in)
  }
}

sealed trait LowPriorityImplicits {

  implicit def FunctionTable[I, O]: Table[Function[I, O], I, O] = new Table[Function[I, O], I, O] {

    override def get(table: Function[I, O])(in: I): Option[O] = Some(table(in))

  }

  implicit def PartialFunctionTable[I, O]: Table[PartialFunction[I, O], I, O] = new Table[PartialFunction[I, O], I, O] {

    override def get(table: PartialFunction[I, O])(in: I): Option[O] = table.lift(in)

  }

}
