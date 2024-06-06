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

package fs2.data.msgpack.low.internal

import fs2.Chunk
import fs2.Pull
import fs2.RaiseThrowable
import fs2.Stream
import fs2.data.msgpack.low.MsgpackItem
import scodec.bits.ByteVector

private[internal] object Helpers {
  case class MsgpackParsingException(str: String) extends Exception

  /** @param chunk Current chunk
    * @param idx Index of the current [[Byte]] in `chunk`
    * @param rest Rest of the stream
    * @param acc Accumulator of which contents are emitted when acquiring a new chunk 
    */
  case class ParserContext[F[_]](chunk: Chunk[Byte], idx: Int, rest: Stream[F, Byte], acc: List[MsgpackItem]) {
    def prepend(item: MsgpackItem) = ParserContext(chunk, idx, rest, item :: acc)
    def next = ParserContext(chunk, idx + 1, rest, acc)
    def toResult[T](result: T) = ParserResult(chunk, idx, rest, acc, result)
  }

  case class ParserResult[F[_], T](chunk: Chunk[Byte], idx: Int, rest: Stream[F, Byte], acc: List[MsgpackItem], result: T) {
    def toContext = ParserContext(chunk, idx, rest, acc)
    def accumulate(op: T => MsgpackItem) = ParserContext(chunk, idx, rest, op(result) :: acc)
  }

  /** Ensures that a computation `cont` will happen inside a valid context.
    * @param cont function to be run with a chunk ensured
    * @param onEos ran when out of stream
    */
  def ensureChunk[F[_], T](ctx: ParserContext[F])(cont: ParserContext[F] => Pull[F, MsgpackItem, T])(
      onEos: => Pull[F, MsgpackItem, T]): Pull[F, MsgpackItem, T] = {
    if (ctx.idx >= ctx.chunk.size) {
      Pull.output(Chunk.from(ctx.acc.reverse)) >> ctx.rest.pull.uncons flatMap {
        case Some((hd, tl)) => ensureChunk(ParserContext(hd, 0, tl, Nil))(cont)(onEos)
        case None           => onEos
      }
    } else {
      cont(ctx)
    }
  }

  def requireOneByte[F[_]](ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserResult[F, Byte]] = {
    ensureChunk(ctx) { ctx =>
      // Inbounds chunk access is guaranteed by `ensureChunk`
      Pull.pure(ctx.next.toResult(ctx.chunk(ctx.idx)))
    } {
      Pull.raiseError(new MsgpackParsingException("Unexpected end of input"))
    }
  }

  def requireBytes[F[_]](count: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserResult[F, ByteVector]] = {
    def go(count: Int, ctx: ParserContext[F], bytes: ByteVector): Pull[F, MsgpackItem, ParserResult[F, ByteVector]] = {
      if (count <= 0) {
        Pull.pure(ctx.toResult(bytes.reverse))
      } else {
        requireOneByte(ctx) flatMap { res =>
          go(count - 1, res.toContext, res.result +: bytes)
        }
      }
    }

    go(count, ctx, ByteVector.empty)
  }
}
