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

package fs2.data.pattern

/** Describes the structure of an expression in term of constructor
  * trees that can be selected.
  */
trait Selectable[Expr, GExpr, Tag] {
  def tree(e: Expr): ConstructorTree[Tag]

  def select(e: Expr, sel: Selector[GExpr, Tag])(implicit evaluator: Evaluator[GExpr, Tag]): Option[Tag] =
    tree(e).select(sel).map(_.tag)

}

trait Evaluator[Expr, Tag] {
  def eval(guard: Expr, tree: ConstructorTree[Tag]): Option[Tag]
}

object Selectable {
  def apply[Expr, GExpr, Tag](implicit ev: Selectable[Expr, GExpr, Tag]): Selectable[Expr, GExpr, Tag] =
    ev
}

case class ConstructorTree[Tag](tag: Tag, args: List[ConstructorTree[Tag]]) {
  def select[Expr](sel: Selector[Expr, Tag])(implicit evaluator: Evaluator[Expr, Tag]): Option[ConstructorTree[Tag]] =
    sel match {
      case Selector.Root => Some(this)
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
