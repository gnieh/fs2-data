package fs2
package data
package json

import text.CharLikeChunks

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
