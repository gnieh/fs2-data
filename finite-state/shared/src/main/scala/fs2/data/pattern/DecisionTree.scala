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

import fs2.data.matching.Table

import scala.annotation.tailrec

sealed trait DecisionTree[Tag, Out]
object DecisionTree {
  case class Fail[Tag, Out]() extends DecisionTree[Tag, Out]
  case class Leaf[Tag, Out](bindings: List[Binding[Selector[Tag]]], out: Out) extends DecisionTree[Tag, Out]
  case class Switch[Tag, Out](on: Selector[Tag],
                              branches: Map[Tag, DecisionTree[Tag, Out]],
                              catchAll: Option[DecisionTree[Tag, Out]])
      extends DecisionTree[Tag, Out]

  implicit def DecisionTreeIsTable[I, Tag, O](implicit I: Selectable[I, Tag]): Table[DecisionTree[Tag, O], I, O] =
    new Table[DecisionTree[Tag, O], I, O] {
      type In = I
      type Out = O

      override def get(tree: DecisionTree[Tag, O])(in: I): Option[O] = {
        val skel = I.tree(in)
        @tailrec
        def loop(tree: DecisionTree[Tag, O]): Option[O] =
          (skel, tree) match {
            case (_, Leaf(bindings, out)) => Some(out)
            case (skel, Switch(on, branches, fallBack)) =>
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
                  None
              }
            case _ => None
          }
        loop(tree)
      }

    }

}
