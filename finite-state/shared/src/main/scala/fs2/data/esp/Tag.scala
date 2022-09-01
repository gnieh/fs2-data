/*
 * Copyright 2019-2022 Lucas Satabin
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

package fs2.data.esp

import fs2.data.pattern.IsTag

sealed trait Tag[+T]
object Tag {
  case object Input extends Tag[Nothing]
  case class State(q: Int) extends Tag[Nothing]
  case class Depth(d: Int) extends Tag[Nothing]
  case class Name[T](name: T) extends Tag[T]
  case object Open extends Tag[Nothing]
  case object Close extends Tag[Nothing]
  case object End extends Tag[Nothing]
  case object Leaf extends Tag[Nothing]
  case class Value[T](v: T) extends Tag[T]

  implicit def TagIsTag[T]: IsTag[Tag[T]] = new IsTag[Tag[T]] {

    def isOpen(tag: Tag[T]) =
      tag match {
        case Input | Open | Close | Leaf | End => false
        case _                                 => true
      }

    override def eqv(x: Tag[T], y: Tag[T]): Boolean =
      x == y

    override def range(tag: Tag[T]): Iterator[Tag[T]] =
      tag match {
        case Input => Iterator(Input)
        case Open  => Iterator(Open)
        case Close => Iterator(Close)
        case Leaf  => Iterator(Leaf)
        case End   => Iterator(End)
        case _     => Iterator.empty
      }

  }

}
