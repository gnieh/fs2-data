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

package fs2
package data
package text

import scala.annotation.implicitNotFound

import java.nio.charset.Charset

/** A typeclass witnessing that a stream of type `In` has chunks
  * that can be iterated over to get characters.
  */
@implicitNotFound("Cannot prove that stream of type ${In} has chunks that can be iterated over to get characters")
trait CharLikeChunks[F[_], In] {

  /** The context used to pull chunks and iterate over characters.
    * Implementations might decide to have immutable or mutable contexts
    * depending on performance constraints that are desired.
    */
  type Context

  /** Creates a context out of a stream. */
  def create(s: Stream[F, In]): Context

  /** Decides whether a new chunk must be pulled from the context.
    */
  def needsPull(ctx: Context): Boolean

  /** Pulls the next chunk from the context. Returns `None` if stream is exhausted. */
  def pullNext(ctx: Context): Pull[F, Nothing, Option[Context]]

  /** Advances one character in the context.
    * This method is called for stepping through characters,
    * so it is preferrable to have it efficient. Implemetations can be based
    * on mutable `Context`, as a new one is created locally by the parsers.
    *
    * This should not perform any effect or pull.
    *
    * Implementations can assume that `needsPull` will be called after this
    * step function to check boundaries within the current chunk in the
    * context.
    */
  def advance(ctx: Context): Context

  /** Returns the current character in the context.
    *
    * Implementations can assume this will never fail,
    * as parsers will check for `needsPull` and `pullNext`
    * before calling this function.
    *
    * This should not perform any effect or pull and must be pure.
    */
  def current(ctx: Context): Char

}

private class CharLikeCharChunks[F[_]] extends CharLikeChunks[F, Char] {

  class CharContext(var chunk: Chunk[Char], var idx: Int, var rest: Stream[F, Char])

  type Context = CharContext

  def create(s: Stream[F, Char]): CharContext = new CharContext(Chunk.empty, 0, s)

  def needsPull(ctx: CharContext): Boolean =
    ctx.idx >= ctx.chunk.size

  def pullNext(ctx: CharContext): Pull[F, Nothing, Option[CharContext]] =
    ctx.rest.pull.uncons.map(_.map { case (hd, tl) =>
      ctx.chunk = hd
      ctx.idx = 0
      ctx.rest = tl
      ctx
    })

  def advance(ctx: Context): Context = {
    ctx.idx += 1
    ctx
  }

  def current(ctx: CharContext): Char =
    ctx.chunk(ctx.idx)

}

private class CharLikeStringChunks[F[_]] extends CharLikeChunks[F, String] {
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

// BEWARE: this implementation only works for single-byte encodings, do not use this for utf-8 for instance
private class CharLikeSingleByteChunks[F[_]](charset: Charset) extends CharLikeChunks[F, Byte] {

  class ByteContext(var chunk: String, var idx: Int, var rest: Stream[F, Byte])

  type Context = ByteContext

  def create(s: Stream[F, Byte]): Context =
    new ByteContext("", 0, s)

  def needsPull(ctx: Context): Boolean =
    ctx.idx >= ctx.chunk.size

  def pullNext(ctx: Context): Pull[F, Nothing, Option[Context]] =
    ctx.rest.pull.uncons.map(_.map { case (hd, tl) =>
      // This code is pure. The constructor replaces malformed input
      // with the charset default replacement string, so this never throws
      ctx.chunk = new String(hd.toArray[Byte], charset)
      ctx.idx = 0
      ctx.rest = tl
      ctx
    })

  def advance(ctx: Context): Context = {
    ctx.idx += 1
    ctx
  }

  def current(ctx: Context): Char =
    ctx.chunk(ctx.idx)

}

object CharLikeChunks {

  implicit def charStreamCharLike[F[_]]: CharLikeChunks[F, Char] =
    new CharLikeCharChunks[F]

  implicit def stringStreamCharLike[F[_]]: CharLikeChunks[F, String] =
    new CharLikeStringChunks[F]

}
