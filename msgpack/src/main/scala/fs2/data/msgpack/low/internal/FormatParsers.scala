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
    requireBytes(length, ctx).map { res =>
      res.accumulate(bytes => lift(bytes))
    }
  }

  def parseArray[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(length, ctx).map { res =>
      res.accumulate(v => MsgpackItem.Array(v.toInt(false, ByteOrdering.BigEndian)))
    }
  }

  def parseMap[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(length, ctx).map { res =>
      res.accumulate(v => MsgpackItem.Map(v.toInt(false, ByteOrdering.BigEndian)))
    }
  }

  def parseTimestamp[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    length match {
      case 4 =>
        requireBytes(4, ctx).map { res =>
          res.accumulate(v => MsgpackItem.Timestamp32(v))
        }
      case 8 =>
        requireBytes(8, ctx).map { res =>
          val result = res.result
          val seconds = result & hex"00000003ffffffff"
          val nanosec = result >> 34

          res.toContext.prepend(MsgpackItem.Timestamp64(nanosec.drop(4), seconds.drop(3)))
        }
      case 12 =>
        for {
          res <- requireBytes(4, ctx)
          nanosec = res.result
          res <- requireBytes(8, res.toContext)
          seconds = res.result
        } yield res.toContext.prepend(MsgpackItem.Timestamp96(nanosec, seconds))
      case _ => Pull.raiseError(new MsgpackParsingException(s"Invalid timestamp length: ${length}"))
    }
  }

  def parseFixExt[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireOneByte(ctx).flatMap { res =>
      val header = res.result
      if (header == Headers.Timestamp) {
        parseTimestamp(length, res.toContext)
      } else {
        requireBytes(length, res.toContext).map { res =>
          res.accumulate(bytes => MsgpackItem.Extension(header, bytes))
        }
      }
    }
  }

  def parsePlainExt[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    for {
      res <- requireBytes(length, ctx)
      size = res.result.toInt(false, ByteOrdering.BigEndian)
      res <- requireOneByte(res.toContext)
      header = res.result
      out <-
        if (header == Headers.Timestamp) {
          parseTimestamp(size, res.toContext)
        } else {
          requireBytes(size, res.toContext).map { res =>
            res.accumulate(bytes => MsgpackItem.Extension(header, bytes))
          }
        }
    } yield out
  }

  def parseBin[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(length, ctx).flatMap { res =>
      requireBytes(res.result.toInt(false, ByteOrdering.BigEndian), res.toContext).map { res =>
        res.accumulate(bytes => MsgpackItem.Bin(bytes))
      }
    }
  }

  def parseString[F[_]](length: Int, ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(length, ctx).flatMap { res =>
      requireBytes(res.result.toInt(false, ByteOrdering.BigEndian), res.toContext).map { res =>
        res.accumulate(bytes => MsgpackItem.Str(bytes))
      }
    }
  }
}
