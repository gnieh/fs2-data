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
package low
package internal

import fs2.data.msgpack.low.internal.Helpers._
import fs2.data.msgpack.low.internal.FormatParsers._
import scodec.bits._
import scala.annotation.switch

private[low] object ItemParser {

  /** Parses a single item from the stream.
    */
  private def parseItem[F[_]](ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireOneByte(ctx).flatMap { res =>
      val byte = res.result
      val ctx = res.toContext

      ((byte & 0xff): @switch) match {
        case Headers.Nil       => Pull.pure(ctx.prepend(MsgpackItem.Nil))
        case Headers.NeverUsed => Pull.raiseError(MsgpackMalformedByteStreamException("Reserved value 0xc1 used"))
        case Headers.False     => Pull.pure(ctx.prepend(MsgpackItem.False))
        case Headers.True      => Pull.pure(ctx.prepend(MsgpackItem.True))
        case Headers.Bin8      => parseBin(1, ctx)
        case Headers.Bin16     => parseBin(2, ctx)
        case Headers.Bin32     => parseBin(4, ctx)
        case Headers.Ext8      => parsePlainExt(1, ctx)
        case Headers.Ext16     => parsePlainExt(2, ctx)
        case Headers.Ext32     => parsePlainExt(4, ctx)
        case Headers.Float32   => parseFloat32(ctx)
        case Headers.Float64   => parseFloat64(ctx)
        case Headers.Uint8     => parseSimpleType(MsgpackItem.UnsignedInt(_))(1, ctx)
        case Headers.Uint16    => parseSimpleType(MsgpackItem.UnsignedInt(_))(2, ctx)
        case Headers.Uint32    => parseSimpleType(MsgpackItem.UnsignedInt(_))(4, ctx)
        case Headers.Uint64    => parseSimpleType(MsgpackItem.UnsignedInt(_))(8, ctx)
        case Headers.Int8      => parseSimpleType(MsgpackItem.SignedInt(_))(1, ctx)
        case Headers.Int16     => parseSimpleType(MsgpackItem.SignedInt(_))(2, ctx)
        case Headers.Int32     => parseSimpleType(MsgpackItem.SignedInt(_))(4, ctx)
        case Headers.Int64     => parseSimpleType(MsgpackItem.SignedInt(_))(8, ctx)
        case Headers.FixExt1   => parseFixExt(1, ctx)
        case Headers.FixExt2   => parseFixExt(2, ctx)
        case Headers.FixExt4   => parseFixExt(4, ctx)
        case Headers.FixExt8   => parseFixExt(8, ctx)
        case Headers.Str8      => parseString(1, ctx)
        case Headers.Str16     => parseString(2, ctx)
        case Headers.Str32     => parseString(4, ctx)
        case Headers.Array16   => parseArray(2, ctx)
        case Headers.Array32   => parseArray(4, ctx)
        case Headers.Map16     => parseMap(2, ctx)
        case Headers.Map32     => parseMap(4, ctx)

        case _ => {
          // Positive fixint
          if ((byte & 0x80) == 0) {
            Pull.pure(ctx.prepend(MsgpackItem.SignedInt(ByteVector(byte))))
          }

          // fixmap
          else if ((byte & 0xf0) == 0x80) {
            val length = byte & 0x0f // 0x8f- 0x80
            Pull.pure(ctx.prepend(MsgpackItem.Map(length.toLong)))
          }

          // fixarray
          else if ((byte & 0xf0) == 0x90) {
            val length = byte & 0x0f // 0x9f- 0x90
            Pull.pure(ctx.prepend(MsgpackItem.Array(length.toLong)))
          }

          // fixstr
          else if ((byte & 0xe0) == 0xa0) {
            val length = byte & 0x1f
            requireBytes(length, ctx) map { res =>
              res.accumulate(MsgpackItem.Str(_))
            }
          }

          // Negatve fixint
          else if ((byte & 0xe0) == 0xe0) {
            Pull.pure(ctx.prepend(MsgpackItem.SignedInt(ByteVector(byte))))
          } else {
            Pull.raiseError(MsgpackMalformedByteStreamException(s"Invalid type ${byte}"))
          }
        }
      }
    }
  }

  def pipe[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Byte, MsgpackItem] = { stream =>
    def go(ctx: ParserContext[F]): Pull[F, MsgpackItem, Unit] = {
      ensureChunk(ctx)(parseItem(_).flatMap(go))(Pull.done)
    }

    go(ParserContext(Chunk.empty, 0, stream, Nil)).stream
  }
}
