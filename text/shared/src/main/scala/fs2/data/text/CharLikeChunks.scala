/*
 * Copyright 2024 fs2-data Project
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

import org.typelevel.scalaccompat.annotation.*

import java.nio.charset.Charset
import scala.annotation.implicitNotFound

/** A typeclass witnessing that a stream of type `In` has chunks
  * that can be iterated over to get characters.
  */
@implicitNotFound("Cannot prove that stream of type ${In} has chunks that can be iterated over to get characters")
@deprecatedInheritance(
  message =
    "Custom implementations may lead to performance issues. This trait will be made sealed in the future. Use the fs2-data provided instances instead",
  since = "fs2-data 1.8.0"
)
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

@nowarn3("cat=deprecation")
sealed trait AsCharBuffer[F[_], T] extends CharLikeChunks[F, T] {

  def mark(ctx: Context): Unit

  def appendMarked(ctx: Context, acc: StringBuilder): Unit

}

private[text] class CharArrayContext[F[_], T](var chunk: Array[Char],
                                              var idx: Int,
                                              var rest: Stream[F, T],
                                              var mark: Int)

private[text] abstract class CharArrayBuffer[F[_], T] extends AsCharBuffer[F, T] {

  type Context = CharArrayContext[F, T]

  def create(s: Stream[F, T]): Context = new Context(Array.empty, 0, s, 0)

  def needsPull(ctx: Context): Boolean =
    ctx.idx >= ctx.chunk.length

  def advance(ctx: Context): Context = {
    ctx.idx += 1
    ctx
  }

  def current(ctx: Context): Char =
    ctx.chunk(ctx.idx)

  override def mark(ctx: Context): Unit =
    ctx.mark = ctx.idx

  override def appendMarked(ctx: Context, acc: StringBuilder): Unit =
    acc.appendAll(ctx.chunk, ctx.mark, ctx.idx - ctx.mark)

}

private[text] class CharLikeCharChunks[F[_]] extends CharArrayBuffer[F, Char] {

  def pullNext(ctx: Context): Pull[F, Nothing, Option[Context]] =
    ctx.rest.pull.uncons.map(_.map { case (hd, tl) =>
      ctx.chunk = hd.toArray
      ctx.idx = 0
      ctx.rest = tl
      ctx.mark = 0
      ctx
    })

}

private[text] class CharLikeStringChunks[F[_]] extends CharArrayBuffer[F, String] {

  def pullNext(ctx: Context): Pull[F, Nothing, Option[Context]] =
    ctx.rest.pull.uncons1.map(_.map { case (hd, tl) =>
      ctx.chunk = hd.toCharArray()
      ctx.idx = 0
      ctx.rest = tl
      ctx.mark = 0
      ctx
    })

}

// BEWARE: this implementation only works for single-byte encodings, do not use this for utf-8 for instance
private[text] class CharLikeSingleByteChunks[F[_]](charset: Charset) extends CharArrayBuffer[F, Byte] {

  def pullNext(ctx: Context): Pull[F, Nothing, Option[Context]] =
    ctx.rest.pull.uncons.map(_.map { case (hd, tl) =>
      // This code is pure. The constructor replaces malformed input
      // with the charset default replacement string, so this never throws
      ctx.chunk = new String(hd.toArray[Byte], charset).toCharArray()
      ctx.idx = 0
      ctx.rest = tl
      ctx.mark = 0
      ctx
    })

}

private[text] class CharLikeUtf8ByteChunks[F[_]] extends AsCharBuffer[F, Byte] {
  val stringsCharBuffer = CharLikeChunks.stringStreamCharBuffer[F]
  override type Context = stringsCharBuffer.Context
  override def create(s: Stream[F, Byte]): Context = stringsCharBuffer.create(s.through(fs2.text.utf8.decode))
  override def needsPull(ctx: Context): Boolean = stringsCharBuffer.needsPull(ctx)
  override def pullNext(ctx: Context): Pull[F, Nothing, Option[Context]] = stringsCharBuffer.pullNext(ctx)
  override def advance(ctx: Context): Context = stringsCharBuffer.advance(ctx)
  override def current(ctx: Context): Char = stringsCharBuffer.current(ctx)
  override def mark(ctx: Context): Unit = stringsCharBuffer.mark(ctx)
  override def appendMarked(ctx: Context, acc: StringBuilder): Unit = stringsCharBuffer.appendMarked(ctx, acc)
}

object CharLikeChunks {

  @deprecated(message = "use `CharLikeChunks.charStreamCharBuffer` instead", since = "fs2-data 1.8.0")
  def charStreamCharLike[F[_]]: CharLikeChunks[F, Char] =
    charStreamCharBuffer[F]

  implicit def charStreamCharBuffer[F[_]]: AsCharBuffer[F, Char] =
    new CharLikeCharChunks[F]

  @deprecated(message = "use `CharLikeChunks.charStreamCharBuffer` instead", since = "fs2-data 1.8.0")
  def stringStreamCharLike[F[_]]: CharLikeChunks[F, String] =
    stringStreamCharBuffer

  implicit def stringStreamCharBuffer[F[_]]: AsCharBuffer[F, String] =
    new CharLikeStringChunks[F]

}
