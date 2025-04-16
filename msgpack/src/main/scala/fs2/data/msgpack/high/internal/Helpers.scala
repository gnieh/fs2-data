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
package msgpack
package high
package internal

import scodec.bits.ByteVector
import fs2.data.msgpack.low.MsgpackItem

private[high] object Helpers {
  case class DecodingContext[F[_]](chunk: Chunk[low.MsgpackItem], idx: Int, rest: Stream[F, low.MsgpackItem]) {
    @inline def next = DecodingContext(chunk, idx + 1, rest)

    /** Advances decoder position and boundles the context with `result`
      *
      * @param result value to be bundled with the decoding context
      */
    @inline def proceed[A](result: A): DecodingResult[F, A] = Pull.pure((result, DecodingContext(chunk, idx + 1, rest)))
  }

  /** @alias Pull[F, Nothing, (A, DecodingContext[F])]
    * @tparam F Effect type
    * @tparam A Result type
    */
  type DecodingResult[F[_], A] = Pull[F, Nothing, (A, DecodingContext[F])]

  @inline def get1[F[_]: RaiseThrowable, A](ctx: DecodingContext[F])(
      lift: (MsgpackItem, DecodingContext[F]) => DecodingResult[F, A]
  ): DecodingResult[F, A] =
    if (ctx.idx >= ctx.chunk.size) {
      ctx.rest.pull.uncons.flatMap {
        case Some((hd, tl)) => get1(DecodingContext(hd, 0, tl))(lift)
        case None           => Pull.raiseError(new MsgpackUnexpectedEndOfStreamException)
      }
    } else lift(ctx.chunk(ctx.idx), ctx)

  @inline def firstBitPositive(bytes: ByteVector) = (bytes.head & 0x80) == 0x80
}
