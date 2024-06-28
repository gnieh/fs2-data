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
          res.accumulate(v => MsgpackItem.Timestamp32(v.toInt(false)))
        }
      case 8 =>
        requireBytes(8, ctx).map { res =>
          res.accumulate(v => MsgpackItem.Timestamp64(v.toLong(false)))
        }
      case 12 =>
        for {
          res <- requireBytes(4, ctx)
          nanosec = res.result.toInt(false)
          res <- requireBytes(8, res.toContext)
          seconds = res.result.toLong(false)
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

  def parseFloat32[F[_]](ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(4, ctx).map {
      _.accumulate { v =>
        MsgpackItem.Float32 {
          val raw = v.toInt(false)
          val sign = if ((raw & 0x80000000) == 0x80000000) -1 else 1
          val biasedExponent = (raw & 0x7f800000) >>> 23

          // subnormal or zero
          if (biasedExponent == 0) {
            val mantissa = (raw & 0x007fffff).toFloat
            if (mantissa == 0) 0F
            else sign * Math.pow(2, -126).toFloat * (mantissa / 0x800000)
            // Inf or NaN
          } else if (biasedExponent == 0xff) {
            val mantissa = raw & 0x007fffff
            if (mantissa == 0) sign * Float.PositiveInfinity
            else Float.NaN
            // normal
          } else {
            val exponent = (biasedExponent - 127).toDouble
            val mantissa = (raw & 0x007fffff).toFloat + 0x800000
            sign * Math.pow(2, exponent).toFloat * (mantissa / 0x800000)
          }
        }
      }
    }
  }

  def parseFloat64[F[_]](ctx: ParserContext[F])(implicit
      F: RaiseThrowable[F]): Pull[F, MsgpackItem, ParserContext[F]] = {
    requireBytes(8, ctx).map {
      _.accumulate { v =>
        MsgpackItem.Float64 {
          val raw = v.toLong(false)
          val sign = if ((raw & 0x8000000000000000L) == 0x8000000000000000L) -1 else 1
          val biasedExponent = (raw & 0x7ff0000000000000L) >>> 52

          // subnormal or zero
          if (biasedExponent == 0) {
            val mantissa = (raw & 0xfffffffffffffL).toDouble
            if (mantissa == 0) 0D
            else sign * Math.pow(2, -1022) * (mantissa / 0x10000000000000L)
            // Inf or NaN
          } else if (biasedExponent == 0x7ff) {
            val mantissa = raw & 0xfffffffffffffL
            if (mantissa == 0) sign * Double.PositiveInfinity
            else Double.NaN
            // normal
          } else {
            val exponent = (biasedExponent - 1023).toDouble
            val mantissa = (raw & 0xfffffffffffffL).toDouble + 0x10000000000000L
            sign * Math.pow(2, exponent) * (mantissa / 0x10000000000000L)
          }
        }
      }
    }
  }
}
