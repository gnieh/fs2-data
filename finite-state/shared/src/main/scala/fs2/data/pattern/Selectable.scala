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
trait Selectable[Expr, Tag] {
  def tree(e: Expr): ConstructorTree[Tag]

  def select(e: Expr, sel: Selector[Tag]): Option[Tag] =
    tree(e).select(sel).map(_.tag)

}

object Selectable {
  def apply[Expr, Tag](implicit ev: Selectable[Expr, Tag]): Selectable[Expr, Tag] =
    ev
}

case class ConstructorTree[Tag](tag: Tag, args: List[ConstructorTree[Tag]]) {
  def select(sel: Selector[Tag]): Option[ConstructorTree[Tag]] =
    sel match {
      case Selector.Root => Some(this)
      case Selector.Sel(parent, tag, n) =>
        select(parent).flatMap { const =>
          if (const.tag == tag)
            const.args.lift(n)
          else
            None
        }
    }
}
object ConstructorTree {
  def noArgConstructor[Tag](tag: Tag): ConstructorTree[Tag] =
    ConstructorTree(tag, Nil)
}
