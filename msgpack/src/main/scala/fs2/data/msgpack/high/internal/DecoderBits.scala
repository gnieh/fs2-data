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

import fs2.data.msgpack.high.internal.Helpers._
import scodec.bits._
import scala.collection.mutable
import scala.collection.immutable.HashMap

/** Decoding parts shared by the static and the dynamic decoding APIs.
  */
private[high] object DecoderBits {
  def runBigInt[F[_]: RaiseThrowable](bytes: ByteVector, ctx: DecodingContext[F]) =
    /* Msgpack spec defines numbers as 64bit values and although BigInt can
     * certainly support bigger numbers, we don't want to allow malformed data
     * to be decoded.
     */
    if (bytes.length > 8) {
      Pull.raiseError(new MsgpackMalformedItemException("Number exceeds 64 bits"))
    } else {
      val n = BigInt(bytes.toArray)
      ctx.proceed(n)
    }

  def runUnsignedLong[F[_]: RaiseThrowable](bytes: ByteVector, ctx: DecodingContext[F]) =
    if (bytes.length > 8)
      Pull.raiseError(new MsgpackMalformedItemException("Number exceeds 64 bits"))
    else if (bytes.length == 8 && firstBitPositive(bytes))
      Pull.raiseError(new MsgpackDecodingTypeMismatchException("uint bigger than Long.MaxValue", "Long"))
    else
      ctx.proceed(bytes.toLong(false))

  def runSignedLong[F[_]: RaiseThrowable](bytes: ByteVector, ctx: DecodingContext[F]) =
    if (bytes.length > 8)
      Pull.raiseError(new MsgpackMalformedItemException("Number exceeds 64 bits"))
    else
      ctx.proceed(bytes.toLong(true))

  def runUnsignedInt[F[_]: RaiseThrowable](bytes: ByteVector, ctx: DecodingContext[F]) =
    if (bytes.length > 8)
      Pull.raiseError(new MsgpackMalformedItemException("Number exceeds 64 bits"))
    else if (bytes.length > 4)
      Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"${bytes.length}-byte value", "Int"))
    else if (bytes.length == 4 && firstBitPositive(bytes))
      Pull.raiseError(new MsgpackDecodingTypeMismatchException("uint bigger than Int.MaxValue", "Int"))
    else
      ctx.proceed(bytes.toInt(false))

  def runSignedInt[F[_]: RaiseThrowable](bytes: ByteVector, ctx: DecodingContext[F]) =
    if (bytes.length > 8)
      Pull.raiseError(new MsgpackMalformedItemException("Number exceeds 64 bits"))
    else if (bytes.length > 4)
      Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"${bytes.length}-byte value", "Int"))
    else
      ctx.proceed(bytes.toInt(true))

  def runUnsignedShort[F[_]: RaiseThrowable](bytes: ByteVector, ctx: DecodingContext[F]) =
    if (bytes.length > 8)
      Pull.raiseError(new MsgpackMalformedItemException("Number exceeds 64 bits"))
    else if (bytes.length > 2)
      Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"${bytes.length}-byte value", "Short"))
    else if (bytes.length == 2 && firstBitPositive(bytes))
      Pull.raiseError(new MsgpackDecodingTypeMismatchException("uint bigger than Short.MaxValue", "Short"))
    else
      ctx.proceed(bytes.toShort(false))

  def runSignedShort[F[_]: RaiseThrowable](bytes: ByteVector, ctx: DecodingContext[F]) =
    if (bytes.length > 8)
      Pull.raiseError(new MsgpackMalformedItemException("Number exceeds 64 bits"))
    else if (bytes.length > 2)
      Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"${bytes.length}-byte value", "Short"))
    else
      ctx.proceed(bytes.toShort(true))

  def runUnsignedByte[F[_]: RaiseThrowable](bytes: ByteVector, ctx: DecodingContext[F]) =
    if (bytes.length > 8)
      Pull.raiseError(new MsgpackMalformedItemException("Number exceeds 64 bits"))
    else if (bytes.length > 1)
      Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"${bytes.length}-byte value", "Byte"))
    else if (bytes.length == 1 && firstBitPositive(bytes))
      Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"uint bigger than Byte.MaxValue", "Byte"))
    else
      ctx.proceed(bytes.toByte(false))

  def runSignedByte[F[_]: RaiseThrowable](bytes: ByteVector, ctx: DecodingContext[F]) =
    if (bytes.length > 8)
      Pull.raiseError(new MsgpackMalformedItemException("Number exceeds 64 bits"))
    else if (bytes.length > 1)
      Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"${bytes.length}-byte value", "Byte"))
    else
      ctx.proceed(bytes.toByte(true))

  def runList[F[_]: RaiseThrowable, A](size: Int, ctx: DecodingContext[F])(implicit pa: MsgpackDecoder[A]) = {
    def go(n: Int, acc: mutable.Builder[A, List[A]], ctx: DecodingContext[F]): DecodingResult[F, List[A]] = {
      if (n == 0)
        Pull.pure((acc.result(), ctx))
      else
        pa.run(ctx).flatMap { case (item, ctxP) =>
          go(n - 1, acc.addOne(item), ctxP)
        }
    }
    go(size, List.newBuilder[A], ctx.next)
  }

  def runMap[F[_]: RaiseThrowable, K, V](size: Int, ctx: DecodingContext[F])(implicit
      pk: MsgpackDecoder[K],
      pv: MsgpackDecoder[V]) = {
    def go(n: Int,
           acc: mutable.Builder[(K, V), HashMap[K, V]],
           ctx: DecodingContext[F]): DecodingResult[F, Map[K, V]] = {
      if (n == 0)
        Pull.pure((acc.result(), ctx))
      else
        pk.run(ctx).flatMap { case (key, ctxK) =>
          pv.run(ctxK).flatMap { case (value, ctxV) =>
            go(n - 1, acc.addOne((key, value)), ctxV)
          }
        }
    }

    go(size, HashMap.newBuilder[K, V], ctx.next)
  }

  def runString[F[_]: RaiseThrowable](bytes: ByteVector, ctx: DecodingContext[F]) =
    bytes.decodeUtf8 match {
      case Left(e)    => Pull.raiseError(e)
      case Right(str) => ctx.proceed(str)
    }

  def runTimestamp64[F[_]: RaiseThrowable](combined: Long, ctx: DecodingContext[F]) = {
    val seconds = combined & (2 ^ 34 - 1) // lower 34 bits
    val nanoseconds = combined & ((2 ^ 30 - 1) << 34) // uper 30 bits
    val instant = java.time.Instant.ofEpochSecond(seconds, nanoseconds)
    ctx.proceed(instant)
  }
}
