package fs2
package data
package benchmarks

import text._

private class CharLikeStringChunks161[F[_]] extends CharLikeChunks[F, String] {
  class StringContext(var string: String, var sidx: Int, var rest: Stream[F, String])

  type Context = StringContext

  def create(s: Stream[F, String]): Context =
    new StringContext("", 0, s)

  def needsPull(ctx: StringContext): Boolean =
    ctx.sidx >= ctx.string.size

  def pullNext(ctx: StringContext): Pull[F, Nothing, Option[StringContext]] =
    ctx.rest.pull.uncons1.map(_.map { case (hd, tl) =>
      ctx.string = hd
      ctx.sidx = 0
      ctx.rest = tl
      ctx
    })

  def advance(ctx: Context): Context = {
    ctx.sidx += 1
    ctx
  }

  def current(ctx: StringContext): Char =
    ctx.string(ctx.sidx)

}
