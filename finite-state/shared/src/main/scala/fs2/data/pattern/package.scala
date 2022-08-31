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

package fs2.data

import cats.syntax.all._

import scala.annotation.tailrec

package object pattern {

  type Matrix[Tag, Pat, Out] = List[Row[Tag, Pat, Out]]

  implicit class MatrixOps[Tag, Pat, Out](val m: Matrix[Tag, Pat, Out]) extends AnyVal {

    def verticalView: VMatrix[Tag, Pat, Out] =
      VMatrix(m.map(_.patterns).transpose.map(Col(_)),
              m.map({ case Row(pat, bindings, _, out) => patterns => Row(pat, bindings, patterns, out) }))

  }

  type Binding[Expr] = (Option[String], Expr)

  private[pattern] def headConstructors[Tag](skels: List[Skeleton[Tag]]): List[Skeleton.Constructor[Tag]] = {
    @tailrec
    def go(skel: Skeleton[Tag]): Option[Skeleton.Constructor[Tag]] =
      skel match {
        case Skeleton.Constructor(tag, args) => Skeleton.Constructor(tag, args).some
        case Skeleton.Wildcard(_)            => none
        case Skeleton.As(p, _)               => go(p)
      }
    skels.flatMap(go(_))
  }

}
