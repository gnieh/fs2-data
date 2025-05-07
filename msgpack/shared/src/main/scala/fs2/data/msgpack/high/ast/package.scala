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
package data.msgpack
package high

import fs2.data.msgpack.low.MsgpackItem
import fs2.data.msgpack.high.internal.Helpers._
import fs2.data.msgpack.high.internal.DeserializerBits._

package object ast {
  implicit object msgpackValueDeserializer extends MsgpackDeserializer[MsgpackValue] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.SignedInt(bytes) =>
          runSignedLong(bytes, ctx).map { case (item, ctx) => (MsgpackValue.Integer(item), ctx) }

        case MsgpackItem.UnsignedInt(bytes) =>
          runUnsignedLong(bytes, ctx).map { case (item, ctx) => (MsgpackValue.Integer(item), ctx) }

        case MsgpackItem.Float32(x) => ctx.proceed(MsgpackValue.Float(x))

        case MsgpackItem.Float64(x) => ctx.proceed(MsgpackValue.Double(x))

        case MsgpackItem.Array(size) if size < 0 =>
          Pull.raiseError(new MsgpackMalformedItemException(s"Negative array length ${size}"))

        case MsgpackItem.Array(size) if size > Int.MaxValue =>
          Pull.raiseError(new MsgpackMalformedItemException("Array size exceeds Int.MaxValue"))

        case MsgpackItem.Array(size) =>
          runList[F, MsgpackValue](size.toInt, ctx).map { case (item, ctx) =>
            (MsgpackValue.Array(item), ctx)
          }

        case MsgpackItem.Map(size) if size < 0 =>
          Pull.raiseError(new MsgpackMalformedItemException(s"Negative map length ${size}"))

        case MsgpackItem.Map(size) if size > Int.MaxValue =>
          Pull.raiseError(new MsgpackMalformedItemException("Map size exceeds Int.MaxValue"))

        case MsgpackItem.Map(size) =>
          runMap[F, MsgpackValue, MsgpackValue](size.toInt, ctx).map { case (item, ctx) =>
            (MsgpackValue.Map(item), ctx)
          }

        case MsgpackItem.Timestamp32(seconds) =>
          ctx.proceed(MsgpackValue.Timestamp(0, seconds.toLong))

        case item: MsgpackItem.Timestamp64 =>
          ctx.proceed(MsgpackValue.Timestamp(item.nanoseconds, item.seconds))

        case MsgpackItem.Timestamp96(nanoseconds, seconds) =>
          ctx.proceed(MsgpackValue.Timestamp(nanoseconds, seconds))

        case MsgpackItem.Bin(bytes) => ctx.proceed(MsgpackValue.Bin(bytes))

        case MsgpackItem.Str(bytes) =>
          runString(bytes, ctx).map { case (item, ctx) => (MsgpackValue.String(item), ctx) }

        case MsgpackItem.Extension(tpe, bytes) => ctx.proceed(MsgpackValue.Extension(tpe, bytes))

        case MsgpackItem.Nil   => ctx.proceed(MsgpackValue.Nil)
        case MsgpackItem.True  => ctx.proceed(MsgpackValue.Boolean(true))
        case MsgpackItem.False => ctx.proceed(MsgpackValue.Boolean(false))
      }
    }
  }

  /** Converts a stream of bytes into a stream [[fs2.data.msgpack.low.MsgpackItem MsgpackItem]]s into a stream of
    * [[MsgpackValue$ MsgpackValue]]s.
    *
    * Alias for fromItems[F, [[MsgpackValue$ MsgpackValue]]].
    */
  @inline def valuesFromItems[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, MsgpackItem, MsgpackValue] =
    fromItems[F, MsgpackValue]

  /** Deserializes a stream of bytes into a stream of [[MsgpackValue]]s.
    *
    * Alias for decode[F, [[MsgpackValue$ MsgpackValue]]]
    */
  @inline def values[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Byte, MsgpackValue] = deserialize[F, MsgpackValue]
}
