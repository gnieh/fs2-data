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

sealed trait Skeleton[Tag] {
  @tailrec
  final def isWildcard: Boolean = this match {
    case Skeleton.Wildcard(_)       => true
    case Skeleton.Constructor(_, _) => false
    case Skeleton.As(inner, _)      => inner.isWildcard
  }

}
object Skeleton {
  case class Wildcard[Tag](as: Option[String]) extends Skeleton[Tag]
  case class Constructor[Tag](tag: Tag, args: List[Skeleton[Tag]]) extends Skeleton[Tag]
  case class As[Tag](inner: Skeleton[Tag], as: String) extends Skeleton[Tag]

  def noArgConstructor[Tag](tag: Tag): Skeleton[Tag] =
    Constructor(tag, Nil)
}
