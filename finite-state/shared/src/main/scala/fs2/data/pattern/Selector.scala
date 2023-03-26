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

package fs2.data.pattern

import cats.syntax.either._

import scala.annotation.tailrec

/** A [[Selector]] represents the part of the matched input that is
  * under scrutinee during the pattern match.
  */
sealed trait Selector[Expr, Tag] {
  def tags: List[Either[(Tag, Int), Expr]] = {
    @tailrec
    def loop(sel: Selector[Expr, Tag], acc: List[Either[(Tag, Int), Expr]]): List[Either[(Tag, Int), Expr]] =
      sel match {
        case Selector.Root()                 => acc
        case Selector.Cons(parent, tag, arg) => loop(parent, (tag, arg).asLeft :: acc)
        case Selector.Guard(parent, guard)   => loop(parent, guard.asRight :: acc)
      }
    loop(this, Nil)
  }
}
object Selector {
  case class Root[Expr, Tag]() extends Selector[Expr, Tag]
  case class Cons[Expr, Tag](sel: Selector[Expr, Tag], tag: Tag, n: Int) extends Selector[Expr, Tag]
  case class Guard[Expr, Tag](sel: Selector[Expr, Tag], expr: Expr) extends Selector[Expr, Tag]
}
