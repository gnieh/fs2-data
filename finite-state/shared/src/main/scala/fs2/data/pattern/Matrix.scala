/*
 * Copyright 2022 Lucas Satabin
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

final case class VMatrix[Tag, Pat, Out](columns: List[Col[Tag]],
                                        rebuild: List[List[Skeleton[Tag]] => Row[Tag, Pat, Out]]) {

  def horizontalView: Matrix[Tag, Pat, Out] =
    rebuild
      .zip(columns.map(_.patterns).transpose)
      .map { case (f, ps) => f(ps) }

}

final case class Row[Tag, Pat, Out](origin: Pat,
                                    bindings: List[Binding[Selector[Tag]]],
                                    patterns: List[Skeleton[Tag]],
                                    output: Out) {

  def bind(binding: Binding[Selector[Tag]]): Row[Tag, Pat, Out] =
    copy(bindings = binding :: bindings)

  def isWidcard: Boolean =
    patterns.forall(_.isWildcard)

}

final case class Col[Tag](patterns: List[Skeleton[Tag]]) {

  def constructors: Map[Tag, List[Skeleton[Tag]]] = {
    @tailrec
    def cons(skel: Skeleton[Tag], sig: Map[Tag, List[Skeleton[Tag]]]): Map[Tag, List[Skeleton[Tag]]] =
      skel match {
        case Skeleton.Constructor(tag, args) => sig.updated(tag, args)
        case Skeleton.Wildcard(_)            => sig
        case Skeleton.As(inner, _)           => cons(inner, sig)
      }

    patterns.foldRight(Map.empty[Tag, List[Skeleton[Tag]]])(cons(_, _))
  }

}
