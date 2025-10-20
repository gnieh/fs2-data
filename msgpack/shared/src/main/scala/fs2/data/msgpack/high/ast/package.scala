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
import fs2.data.msgpack.high.DeserializationResult._

package object ast {
  implicit val msgpackValueDeserializer: MsgpackDeserializer[MsgpackValue] = getItem { (head, tail) =>
    head match {
      case MsgpackItem.SignedInt(bytes) =>
        runSignedLong(bytes, tail).mapValue(MsgpackValue.Integer(_))

      case MsgpackItem.UnsignedInt(bytes) =>
        runUnsignedLong(bytes, tail).mapValue(MsgpackValue.Integer(_))

      case MsgpackItem.Float32(x) => Ok(MsgpackValue.Float(x), tail)

      case MsgpackItem.Float64(x) => Ok(MsgpackValue.Double(x), tail)

      case MsgpackItem.Array(size) if size < 0 =>
        Err(s"Negative array length ${size}")

      case MsgpackItem.Array(size) if size > Int.MaxValue =>
        Err("Array size exceeds Int.MaxValue")

      case MsgpackItem.Array(size) =>
        runList[MsgpackValue](size, tail).mapValue(MsgpackValue.Array(_))

      case MsgpackItem.Map(size) if size < 0 =>
        Err(s"Negative map length ${size}")

      case MsgpackItem.Map(size) if size > Int.MaxValue =>
        Err("Map size exceeds Int.MaxValue")

      case MsgpackItem.Map(size) =>
        runMap[MsgpackValue, MsgpackValue](size, tail).mapValue(MsgpackValue.Map(_))

      case MsgpackItem.Timestamp32(seconds) =>
        Ok(MsgpackValue.Timestamp(0, seconds.toLong), tail)

      case item: MsgpackItem.Timestamp64 =>
        Ok(MsgpackValue.Timestamp(item.nanoseconds, item.seconds), tail)

      case MsgpackItem.Timestamp96(nanoseconds, seconds) =>
        Ok(MsgpackValue.Timestamp(nanoseconds, seconds), tail)

      case MsgpackItem.Bin(bytes) => Ok(MsgpackValue.Bin(bytes), tail)

      case MsgpackItem.Str(bytes) =>
        runString(bytes, tail).mapValue(MsgpackValue.String(_))

      case MsgpackItem.Extension(tpe, bytes) => Ok(MsgpackValue.Extension(tpe, bytes), tail)

      case MsgpackItem.Nil   => Ok(MsgpackValue.Nil, tail)
      case MsgpackItem.True  => Ok(MsgpackValue.Boolean(true), tail)
      case MsgpackItem.False => Ok(MsgpackValue.Boolean(false), tail)

      case null => typeMismatch("null", "MsgpackValue")
    }
  }

  /** Converts a stream of [[fs2.data.msgpack.low.MsgpackItem MsgpackItems]] into a stream of
    * [[MsgpackValue$ MsgpackValues]].
    *
    * Alias for fromItems[F, [[MsgpackValue$ MsgpackValue]]].
    */
  @inline def valuesFromItems[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, MsgpackItem, MsgpackValue] =
    fromItems[F, MsgpackValue]

  /** Deserializes a stream of bytes into a stream of [[MsgpackValue$ MsgpackValues]].
    *
    * Alias for deserialize[F, [[MsgpackValue$ MsgpackValue]]]
    */
  @inline def valuesFromBytes[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Byte, MsgpackValue] =
    deserialize[F, MsgpackValue]

  implicit val valueSerializer: MsgpackSerializer[MsgpackValue] = _ match {
    case MsgpackValue.Integer(x)                      => longSerializer(x)
    case MsgpackValue.String(x)                       => stringSerializer(x)
    case MsgpackValue.Bin(x)                          => byteVectorSerializer(x)
    case MsgpackValue.Boolean(x)                      => boolSerializer(x)
    case MsgpackValue.Float(x)                        => floatSerializer(x)
    case MsgpackValue.Double(x)                       => doubleSerializer(x)
    case MsgpackValue.Nil                             => right1(MsgpackItem.Nil)
    case MsgpackValue.Extension(tpe, bytes)           => right1(MsgpackItem.Extension(tpe, bytes))
    case MsgpackValue.Array(x)                        => listSerializer(valueSerializer)(x)
    case MsgpackValue.Map(x)                          => mapSerializer(valueSerializer, valueSerializer)(x)
    case MsgpackValue.Timestamp(nanoseconds, seconds) =>
      // TODO: DRY - similar pattern in Instant and js.Date serializers
      val secondsLow32: Long = seconds & 0xffffffff
      val secondsLow34: Long = seconds & 0x3ffffffffL
      val nanoLow30: Int = nanoseconds & 0x3fffffff

      val item: MsgpackItem =
        if (nanoseconds == 0 && (secondsLow32 == seconds)) {
          MsgpackItem.Timestamp32(seconds.toInt)
        } else if (nanoLow30 == nanoseconds && secondsLow34 == seconds) {
          val combined = (nanoLow30.toLong << 34) | secondsLow34
          MsgpackItem.Timestamp64(combined)
        } else {
          MsgpackItem.Timestamp96(nanoseconds, seconds)
        }

      right1(item)
  }

  /** Serializes a stream of [[MsgpackValue$ MsgpackValues]] into a stream of [[fs2.data.msgpack.low.MsgpackItem MsgpackItems]]
    *
    * Alias for toItems[F, [[MsgpackValue$ MsgpackValue]]]
    */
  @inline def valuesToItems[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, MsgpackValue, MsgpackItem] =
    toItems[F, MsgpackValue]

  /** Serializes a stream of [[MsgpackValue$ MsgpackValues]] into a stream of bytes
    *
    * Alias for serialize[F, [[MsgpackValue$ MsgpackValue]]]
    */
  @inline def valuesToBytes[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, MsgpackValue, Byte] =
    serialize[F, MsgpackValue]
}
