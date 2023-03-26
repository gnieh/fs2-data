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

import scala.annotation.tailrec

sealed trait DecisionTree[Expr, Tag, Out] {
  def get[In](in: In)(implicit In: Selectable[In, Tag], Expr: Evaluator[Expr, Tag]): Option[Out] = {
    val skel = In.tree(in)
    @tailrec
    def loop(tree: DecisionTree[Expr, Tag, Out]): Option[Out] =
      (skel, tree) match {
        case (_, DecisionTree.Leaf(out)) => Some(out)
        case (skel, DecisionTree.Switch(on, branches, fallBack)) =>
          skel.select(on) match {
            case Some(c) =>
              branches.get(c.tag) match {
                case Some(tree) => loop(tree)
                case None =>
                  fallBack match {
                    case Some(tree) => loop(tree)
                    case None       => None
                  }
              }
            case None =>
              fallBack match {
                case Some(tree) => loop(tree)
                case None       => None
              }
          }
        case _ => None
      }
    loop(this)
  }
}

object DecisionTree {
  case class Fail[Expr, Tag, Out]() extends DecisionTree[Expr, Tag, Out]
  case class Leaf[Expr, Tag, Out](out: Out) extends DecisionTree[Expr, Tag, Out]
  case class Switch[Expr, Tag, Out](on: Selector[Expr, Tag],
                                    branches: Map[Tag, DecisionTree[Expr, Tag, Out]],
                                    catchAll: Option[DecisionTree[Expr, Tag, Out]])
      extends DecisionTree[Expr, Tag, Out]
}
