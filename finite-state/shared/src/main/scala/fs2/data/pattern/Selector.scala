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

import scala.annotation.tailrec

/** A [[Selector]] represents the part of the matched input that is
  * under scrutinee during the pattern match.
  */
sealed trait Selector[+Tag] {
  def tags: List[(Tag, Int)] = {
    @tailrec
    def loop(sel: Selector[Tag], acc: List[(Tag, Int)]): List[(Tag, Int)] =
      sel match {
        case Selector.Root                  => acc
        case Selector.Sel(parent, tag, arg) => loop(parent, (tag, arg) :: acc)
      }
    loop(this, Nil)
  }
}
object Selector {
  case object Root extends Selector[Nothing]
  case class Sel[Tag](sel: Selector[Tag], tag: Tag, n: Int) extends Selector[Tag]
}
