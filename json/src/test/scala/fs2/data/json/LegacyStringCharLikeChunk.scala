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

package fs2
package data
package json

import scala.annotation.nowarn

import text.CharLikeChunks

@nowarn
class LegacyStringCharLikeChunk[F[_]] extends CharLikeChunks[F, String] {
  class StringContext(var string: Array[Char], var idx: Int, var rest: Stream[F, String])

  type Context = StringContext

  def create(s: Stream[F, String]): Context =
    new StringContext(Array.empty, 0, s)

  def needsPull(ctx: StringContext): Boolean =
    ctx.idx >= ctx.string.length

  def pullNext(ctx: StringContext): Pull[F, Nothing, Option[StringContext]] =
    ctx.rest.pull.uncons1.map(_.map { case (hd, tl) =>
      ctx.string = hd.toCharArray()
      ctx.idx = 0
      ctx.rest = tl
      ctx
    })

  def advance(ctx: Context): Context = {
    ctx.idx += 1
    ctx
  }

  def current(ctx: StringContext): Char =
    ctx.string(ctx.idx)

}
