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

import scodec.bits._

private[low] object ItemSerializer {
  class MalformedItemError extends Error("item exceeds the maximum size of it's format")
  class MalformedStringError extends MalformedItemError
  class MalformedBinError extends MalformedItemError
  class MalformedIntError extends MalformedItemError
  class MalformedUintError extends MalformedItemError

  private final val positiveIntMask = hex"7f"
  private final val negativeIntMask = hex"e0"

  private final val mapMask = 0x80
  private final val arrayMask = 0x90
  private final val strMask = 0xa0

  /** Checks whether integer `x` fits in `n` bytes. */
  @inline
  private def fitsIn(x: Int, n: Long): Boolean =
    java.lang.Integer.compareUnsigned(x, (Math.pow(2, n.toDouble).toLong - 1).toInt) <= 0

  private case class SerializationContext[F[_]](out: Out[F],
                                                chunk: Chunk[MsgpackItem],
                                                idx: Int,
                                                rest: Stream[F, MsgpackItem])

  /** Buffers [[Chunk]] into 4KiB segments before calling [[Pull.output]].
    *
    * @param contents buffered [[Chunk]]
    */
  private class Out[F[_]](contents: Chunk[Byte]) {
    private val limit = 4096

    /** Pushes `bv` into the buffer and emits the buffer if it reaches the limit.
     */
    @inline
    def push(bv: ByteVector): Pull[F, Byte, Out[F]] =
      if (contents.size >= limit)
        Pull.output(contents).as(new Out(Chunk.byteVector(bv)))
      else
        Pull.done.as(new Out(contents ++ Chunk.byteVector(bv)))

    /** Splices `bv` into segments and pushes them into the buffer while emitting the buffer at the same time so
      * that it never exceeds the limit during the operation.
      *
      * Use this instead of [[Out.push]] when `bv` may significantly exceed 4KiB.
      */
    def pushBuffered(bv: ByteVector): Pull[F, Byte, Out[F]] = {
      @inline
      def go(chunk: Chunk[Byte], rest: ByteVector): Pull[F, Byte, Out[F]] =
        if (rest.isEmpty)
          Pull.done.as(new Out(chunk))
        else
          Pull.output(chunk) >> go(Chunk.byteVector(rest.take(limit.toLong)), rest.drop(limit.toLong))

      if (bv.isEmpty)
        this.push(bv)
      else if (contents.size >= limit)
        Pull.output(contents) >> go(Chunk.byteVector(bv.take(limit.toLong)), bv.drop(limit.toLong))
      else
        go(contents ++ Chunk.byteVector(bv.take(limit.toLong - contents.size)), bv.drop(limit.toLong - contents.size))
    }

    /** Outputs the whole buffer. */
    @inline
    def flush = Pull.output(contents)
  }

  @inline
  private def step[F[_]: RaiseThrowable](o: Out[F], item: MsgpackItem): Pull[F, Byte, Out[F]] = item match {
    case MsgpackItem.UnsignedInt(bytes) =>
      val bs = bytes.dropWhile(_ == 0)
      if (bs.size <= 1)
        o.push(ByteVector(Headers.Uint8) ++ bs.padLeft(1))
      else if (bs.size <= 2)
        o.push(ByteVector(Headers.Uint16) ++ bs.padLeft(2))
      else if (bs.size <= 4)
        o.push(ByteVector(Headers.Uint32) ++ bs.padLeft(4))
      else if (bs.size <= 8)
        o.push(ByteVector(Headers.Uint64) ++ bs.padLeft(8))
      else
        Pull.raiseError(new MalformedUintError)

    case MsgpackItem.SignedInt(bytes) =>
      val bs = bytes.dropWhile(_ == 0)
      if (bs.size <= 1)
        // positive fixint or negative fixint
        if ((bs & positiveIntMask) == bs || (bs & negativeIntMask) == negativeIntMask)
          o.push(bs.padLeft(1))
        else
          o.push(ByteVector(Headers.Int8) ++ bs.padLeft(1))
      else if (bs.size <= 2)
        o.push(ByteVector(Headers.Int16) ++ bs.padLeft(2))
      else if (bs.size <= 4)
        o.push(ByteVector(Headers.Int32) ++ bs.padLeft(4))
      else if (bs.size <= 8)
        o.push(ByteVector(Headers.Int64) ++ bs.padLeft(8))
      else
        Pull.raiseError(new MalformedIntError)

    case MsgpackItem.Float32(float) =>
      o.push(ByteVector(Headers.Float32) ++ ByteVector.fromInt(java.lang.Float.floatToIntBits(float)))

    case MsgpackItem.Float64(double) =>
      o.push(ByteVector(Headers.Float64) ++ ByteVector.fromLong(java.lang.Double.doubleToLongBits(double)))

    case MsgpackItem.Str(bytes) =>
      if (bytes.size <= 31) {
        o.push(ByteVector.fromByte((strMask | bytes.size).toByte) ++ bytes)
      } else if (bytes.size <= Math.pow(2, 8) - 1) {
        val size = ByteVector.fromByte(bytes.size.toByte)
        o.push(ByteVector(Headers.Str8) ++ size ++ bytes)
      } else if (bytes.size <= Math.pow(2, 16) - 1) {
        val size = ByteVector.fromShort(bytes.size.toShort)
        o.push(ByteVector(Headers.Str16) ++ size ++ bytes)
      } else if (fitsIn(bytes.size.toInt, 32)) {
        val size = ByteVector.fromInt(bytes.size.toInt)
        /* Max length of str32 (incl. type and length info) is 2^32 + 4 bytes
         * which is more than Chunk can handle at once
         */
        o.pushBuffered(ByteVector(Headers.Str32) ++ size ++ bytes)
      } else {
        Pull.raiseError(new MalformedStringError)
      }

    case MsgpackItem.Bin(bytes) =>
      if (bytes.size <= Math.pow(2, 8) - 1) {
        val size = ByteVector.fromByte(bytes.size.toByte)
        o.push(ByteVector(Headers.Bin8) ++ size ++ bytes)
      } else if (bytes.size <= Math.pow(2, 16) - 1) {
        val size = ByteVector.fromShort(bytes.size.toShort)
        o.push(ByteVector(Headers.Bin16) ++ size ++ bytes)
      } else if (fitsIn(bytes.size.toInt, 32)) {
        val size = ByteVector.fromInt(bytes.size.toInt)
        /* Max length of str32 (incl. type and length info) is 2^32 + 4 bytes
         * which is more than Chunk can handle at once
         */
        o.pushBuffered(ByteVector(Headers.Bin32) ++ size ++ bytes)
      } else {
        Pull.raiseError(new MalformedBinError)
      }

    case MsgpackItem.Array(size) =>
      if (size <= 15) {
        o.push(ByteVector.fromByte((arrayMask | size).toByte))
      } else if (size <= Math.pow(2, 16) - 1) {
        val s = ByteVector.fromShort(size.toShort)
        o.push(ByteVector(Headers.Array16) ++ s)
      } else {
        val s = ByteVector.fromLong(size, 4)
        o.push(ByteVector(Headers.Array32) ++ s)
      }

    case MsgpackItem.Map(size) =>
      if (size <= 15) {
        o.push(ByteVector.fromByte((mapMask | size).toByte))
      } else if (size <= Math.pow(2, 16) - 1) {
        val s = ByteVector.fromShort(size.toShort)
        o.push(ByteVector(Headers.Map16) ++ s)
      } else {
        val s = ByteVector.fromLong(size, 4)
        o.push(ByteVector(Headers.Map32) ++ s)
      }

    case MsgpackItem.Extension(tpe, bytes) =>
      val bs = bytes.dropWhile(_ == 0)
      if (bs.size <= 1) {
        o.push((ByteVector(Headers.FixExt1) :+ tpe) ++ bs.padLeft(1))
      } else if (bs.size <= 2) {
        o.push((ByteVector(Headers.FixExt2) :+ tpe) ++ bs.padLeft(2))
      } else if (bs.size <= 4) {
        o.push((ByteVector(Headers.FixExt4) :+ tpe) ++ bs.padLeft(4))
      } else if (bs.size <= 8) {
        o.push((ByteVector(Headers.FixExt8) :+ tpe) ++ bs.padLeft(8))
      } else if (bs.size <= 16) {
        o.push((ByteVector(Headers.FixExt16) :+ tpe) ++ bs.padLeft(16))
      } else if (bs.size <= Math.pow(2, 8) - 1) {
        val size = ByteVector.fromByte(bs.size.toByte)
        o.push((ByteVector(Headers.Ext8) ++ size :+ tpe) ++ bs)
      } else if (bs.size <= Math.pow(2, 16) - 1) {
        val size = ByteVector.fromShort(bs.size.toShort)
        o.push((ByteVector(Headers.Ext16) ++ size :+ tpe) ++ bs)
      } else {
        val size = ByteVector.fromInt(bs.size.toInt)
        /* Max length of ext32 (incl. type and length info) is 2^32 + 5 bytes
         * which is more than Chunk can handle at once.
         */
        o.pushBuffered((ByteVector(Headers.Ext32) ++ size :+ tpe) ++ bs)
      }

    case MsgpackItem.Timestamp32(seconds) =>
      o.push((ByteVector(Headers.FixExt4) :+ Headers.Timestamp.toByte) ++ ByteVector.fromInt(seconds))

    case MsgpackItem.Timestamp64(combined) =>
      o.push((ByteVector(Headers.FixExt8) :+ Headers.Timestamp.toByte) ++ ByteVector.fromLong(combined))

    case MsgpackItem.Timestamp96(nanoseconds, seconds) =>
      val ns = ByteVector.fromInt(nanoseconds)
      val s = ByteVector.fromLong(seconds)
      o.push((ByteVector(Headers.Ext8) :+ 12 :+ Headers.Timestamp.toByte) ++ ns ++ s)

    case MsgpackItem.Nil =>
      o.push(ByteVector(Headers.Nil))

    case MsgpackItem.False =>
      o.push(ByteVector(Headers.False))

    case MsgpackItem.True =>
      o.push(ByteVector(Headers.True))
  }

  private def stepChunk[F[_]: RaiseThrowable](ctx: SerializationContext[F]): Pull[F, Byte, SerializationContext[F]] =
    if (ctx.idx >= ctx.chunk.size)
      Pull.done.as(ctx)
    else
      step(ctx.out, ctx.chunk(ctx.idx)).flatMap { out =>
        stepChunk(SerializationContext(out, ctx.chunk, ctx.idx + 1, ctx.rest))
      }

  def pipe[F[_]: RaiseThrowable]: Pipe[F, MsgpackItem, Byte] = { stream =>
    def go(out: Out[F], rest: Stream[F, MsgpackItem]): Pull[F, Byte, Unit] =
      rest.pull.uncons.flatMap {
        case None => out.flush
        case Some((chunk, rest)) =>
          stepChunk(SerializationContext(out, chunk, 0, rest)).flatMap { case SerializationContext(out, _, _, rest) =>
            go(out, rest)
          }
      }

    go(new Out(Chunk.empty), stream).stream
  }
}
