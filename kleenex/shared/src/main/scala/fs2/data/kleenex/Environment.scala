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

package fs2.data.kleenex

case class Environment(stack: List[String], registers: Map[String, String]) {

  /** Appends the `s` on top of the stack. */
  def append(s: String): Option[Environment] =
    stack match {
      case r :: stack => Some(copy((r + s) :: stack))
      case Nil        => None
    }

  /** Pushes an empty value on top of the stack. */
  def push: Environment =
    copy(stack = "" :: stack)

  /** Pops the value on top of the stack and stores it in `reg`. */
  def pop(reg: String): Option[Environment] =
    stack match {
      case r :: stack => Some(copy(stack = stack, registers = registers.updated(reg, r)))
      case Nil        => None
    }

  /** Appends the value in `reg` on top of the stack and empties the register. */
  def write(reg: String): Option[Environment] =
    stack match {
      case r :: stack =>
        val value = registers.getOrElse(reg, "")
        Some(copy(stack = (r + value) :: stack, registers.updated(reg, "")))
      case Nil => None
    }

}
