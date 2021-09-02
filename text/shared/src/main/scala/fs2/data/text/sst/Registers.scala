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

package fs2.data.text.sst

import cats.data.NonEmptyList

/** The register context consists of:
  *  - a working stack for currently worked on elements
  *  - a set of predefined registers.
  * The elements on the stack are in reverse order but the ones in registers are in the correct order and can be worked with directly.
  */
case class Registers(stack: NonEmptyList[List[Value]], registers: Vector[List[Value]]) {

  /** Appends a string to the bottom stack element. */
  def append(s: String): Registers =
    copy(stack = NonEmptyList(Value.Str(s) :: stack.head, stack.tail))

  /** Push a new empty sequence onto the stack. */
  def push: Registers =
    copy(stack = stack.prepend(Nil))

  /** Pops the current bottom stack element into `register`.
    * If the stack has a single element, returns `None` and nothing is changed.
    * If the register doesn't exist, returns `None` as well and registers are left unchanged.
    */
  def pop(register: Int): Option[Registers] =
    if (registers.isDefinedAt(register))
      NonEmptyList.fromList(stack.tail).map { tail =>
        copy(stack = tail, registers = registers.updated(register, stack.head.reverse))
      }
    else
      None

  /** Writes the content of the register onto the bottom element of the stack. */
  def write(register: Int): Option[Registers] =
    if (registers.isDefinedAt(register))
      Some(copy(stack = NonEmptyList(registers(register) reverse_::: stack.head, stack.tail)))
    else
      None

}

sealed trait Value
object Value {
  case class Str(s: String) extends Value
  case class Register(idx: Int) extends Value
}
