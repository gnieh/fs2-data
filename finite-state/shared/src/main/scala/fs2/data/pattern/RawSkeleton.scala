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

sealed trait RawSkeleton[Expr, Tag] {
  val guard: Option[Expr]
}
object RawSkeleton {
  case class Wildcard[Expr, Tag](guard: Option[Expr]) extends RawSkeleton[Expr, Tag]
  case class Constructor[Expr, Tag](tag: Tag, args: List[RawSkeleton[Expr, Tag]], guard: Option[Expr])
      extends RawSkeleton[Expr, Tag]
  def noArgConstructor[Expr, Tag](tag: Tag): RawSkeleton[Expr, Tag] =
    Constructor(tag, Nil, None)
  def wildcard[Expr, Tag]: RawSkeleton[Expr, Tag] =
    Wildcard(None)
}
