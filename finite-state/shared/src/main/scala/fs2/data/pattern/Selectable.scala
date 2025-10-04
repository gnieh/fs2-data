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

package fs2.data.pattern

import cats.Show

/** Describes the structure of an expression in term of constructor
  * trees that can be selected.
  */
trait Selectable[Expr, Tag] {
  def tree(e: Expr): ConstructorTree[Tag]

  def select[GExpr](e: Expr, sel: Selector[GExpr, Tag])(implicit evaluator: Evaluator[GExpr, Tag]): Option[Tag] =
    tree(e).select(sel).map(_.tag)

}

trait Evaluator[Expr, Tag] {
  def eval(guard: Expr, tree: ConstructorTree[Tag]): Option[Tag]
}

object Evaluator {
  implicit def noop[Tag]: Evaluator[NoGuard, Tag] =
    new Evaluator[NoGuard, Tag] {
      override def eval(guard: NoGuard, tree: ConstructorTree[Tag]): Option[Tag] = None
    }
}

/** A sealed trait with no implementation so that it has no inhabitant.
  * To be used when a pattern language has no guards.
  */
sealed trait NoGuard
object NoGuard {
  implicit val show: Show[NoGuard] = Show.show(_ => "")
}

object Selectable {
  def apply[Expr, Tag](implicit ev: Selectable[Expr, Tag]): Selectable[Expr, Tag] =
    ev
}

case class ConstructorTree[Tag](tag: Tag, args: List[ConstructorTree[Tag]]) {
  def select[Expr](sel: Selector[Expr, Tag])(implicit evaluator: Evaluator[Expr, Tag]): Option[ConstructorTree[Tag]] =
    sel match {
      case Selector.Root()               => Some(this)
      case Selector.Cons(parent, tag, n) =>
        select(parent).flatMap { const =>
          if (const.tag == tag)
            const.args.lift(n)
          else
            None
        }
      case Selector.Guard(parent, guard) =>
        select(parent).flatMap(evaluator.eval(guard, _).map(ConstructorTree(_, Nil)))
    }
}
object ConstructorTree {
  def noArgConstructor[Tag](tag: Tag): ConstructorTree[Tag] =
    ConstructorTree(tag, Nil)
}
