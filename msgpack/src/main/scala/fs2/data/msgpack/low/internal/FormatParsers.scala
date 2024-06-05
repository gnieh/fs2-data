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

import fs2.data.msgpack.low.MsgpackItem
import fs2.data.msgpack.low.internal.Helpers._
import scodec.bits._

private[internal] object FormatParsers {
  def parseSimpleType[F[_]](lift: ByteVector => MsgpackItem)(length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(length, ctx) map { case (ctx, result) =>
      ctx.prepend(lift(result))
    }
  }

  def parseUnsignedInt[F[_]](length: Int, ctx: ParserContext[F])(implicit F: RaiseThrowable[F]) =
    parseSimpleType(MsgpackItem.UnsignedInt)(length, ctx)

  def parseArray[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(length, ctx) map { case (ctx, result) =>
      ctx.prepend(MsgpackItem.Array(result.toInt(false, ByteOrdering.BigEndian)))
    }
  }

  def parseMap[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(length, ctx) map { case (ctx, result) =>
      ctx.prepend(MsgpackItem.Map(result.toInt(false, ByteOrdering.BigEndian)))
    }
  }

  def parseTimestamp[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    length match {
      case 4 =>
        requireBytes(4, ctx) map { case (ctx, result) =>
          ctx.prepend(MsgpackItem.Timestamp32(result))
        }
      case 8 =>
        requireBytes(8, ctx) map {
          case (ctx, result) => {
            val seconds = result & hex"00000003ffffffff"
            val nanosec = result >> 34

            ctx.prepend(MsgpackItem.Timestamp64(nanosec.drop(4), seconds.drop(3)))
          }
        }
      case 12 =>
        for {
          (ctx, nanosec) <- requireBytes(4, ctx)
          (ctx, seconds) <- requireBytes(8, ctx)
        } yield ctx.prepend(MsgpackItem.Timestamp96(nanosec, seconds))
      case _ => Pull.raiseError(new MsgpackParsingException(s"Invalid timestamp length: ${length}"))
    }
  }

  def parseFixExt[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireOneByte(ctx) flatMap {
      case (ctx, header) => {
        if (header == Headers.Timestamp) {
          parseTimestamp(length, ctx)
        } else {
          requireBytes(length, ctx) map { case (ctx, bytes) =>
            ctx.prepend(MsgpackItem.Extension(header, bytes))
          }
        }
      }
    }
  }

  def parsePlainExt[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    for {
      (ctx, size) <- requireBytes(length, ctx)
      (ctx, header) <- requireOneByte(ctx)
      sizeI = size.toInt(false, ByteOrdering.BigEndian)
      out <-
        if (header == Headers.Timestamp) {
          parseTimestamp(sizeI, ctx)
        } else {
          requireBytes(sizeI, ctx) map { case (ctx, bytes) =>
            ctx.prepend(MsgpackItem.Extension(header, bytes))
          }
        }
    } yield out
  }

  def parseBin[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(length, ctx) flatMap { case (ctx, result) =>
      requireBytes(result.toInt(false, ByteOrdering.BigEndian), ctx) map { case (ctx, result) =>
        ctx.prepend(MsgpackItem.Bin(result))
      }
    }
  }

  def parseString[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(length, ctx) flatMap { case (ctx, result) =>
      requireBytes(result.toInt(false, ByteOrdering.BigEndian), ctx) map { case (ctx, result) =>
        ctx.prepend(MsgpackItem.Str(result))
      }
    }
  }
}