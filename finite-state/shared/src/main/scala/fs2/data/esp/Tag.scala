/*
 * Copyright 2023 Lucas Satabin
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

import cats.syntax.all._
import cats.{Order, Show}
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

  implicit def show[T: Show]: Show[Tag[T]] = {
    case Input      => "$input"
    case State(q)   => show"q$q"
    case Depth(d)   => show"[$d]"
    case Name(name) => show"<$name>"
    case Open       => "<%>"
    case Close      => "</%>"
    case End        => "$"
    case Leaf       => "%"
    case Value(v)   => show"$v"
  }

  implicit def order[T: Order]: Order[Tag[T]] = Order.from {
    case (Name(n1), Name(n2))                      => Order[T].compare(n1, n2)
    case (Name(_), _)                              => -1
    case (Open, Name(_))                           => 1
    case (Open, _)                                 => -1
    case (Close, Name(_) | Open)                   => 1
    case (Close, _)                                => -1
    case (Value(_), Name(_) | Open | Close)        => 1
    case (Value(v1), Value(v2))                    => Order[T].compare(v1, v2)
    case (Value(_), Leaf)                          => -1
    case (Leaf, Value(_) | Name(_) | Open | Close) => 1
    case (Leaf, _)                                 => -1
    case (_, _)                                    => 1
  }

}
