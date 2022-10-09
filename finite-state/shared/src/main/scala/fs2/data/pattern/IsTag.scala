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

import cats._

trait IsTag[Tag] extends Eq[Tag] {

  /** Indicates whether this tag is open.
    * For instance, tags representing integers are open, strings as well.
    */
  def isOpen(tag: Tag): Boolean

  /** The range of this tag type, i.e. all possible values.
    * If the tag is open, returns an empty iterator.
    */
  def range(tag: Tag): Iterator[Tag]

  final def hasUnmatchedConstructor(skel: Skeleton[_, Tag], matched: Set[Tag]): Boolean =
    skel match {
      case Skeleton.Constructor(tag, _, _) => isOpen(tag) || range(tag).exists(!matched.contains(_))
      case Skeleton.Wildcard(_)            => false
      case Skeleton.Guard(Some(_))         => true
      case Skeleton.Guard(None)            => false
    }

}
