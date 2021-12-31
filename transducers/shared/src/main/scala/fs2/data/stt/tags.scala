/*
 * Copyright 2021 Lucas Satabin
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

package fs2.data.stt

import scala.annotation.implicitNotFound

sealed trait Tag {
  def unapply[C](c: C)(implicit C: HasTag[C]): Boolean =
    C.tag(c) == this
}
object Tag {
  case object Call extends Tag
  case object Return extends Tag
  case object Internal extends Tag
}

/** Typeclass indicating that the characters of type `C`
  * can be tagged as:
  *  - call (e.g. opening tag),
  *  - return (e.g. closing tag),
  *  - internal (non structuring character).
  */
@implicitNotFound(
  "Cannot prove that type ${C} has tags. Make sure to provide an implicit instance of `fs2.data.stt.HasTag[${C}]` in scope")
trait HasTag[C] {
  def tag(c: C): Tag

  def unapply(c: C): Some[Tag] = Some(tag(c))
}
