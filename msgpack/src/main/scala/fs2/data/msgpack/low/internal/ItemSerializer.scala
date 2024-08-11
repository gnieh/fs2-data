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
  def compressed: MsgpackItem => ByteVector = {
    case MsgpackItem.UnsignedInt(bytes) =>
      if (bytes.size <= 1)
        ByteVector(Headers.Uint8).buffer ++ bytes.padLeft(1)
      else if (bytes.size <= 2)
        ByteVector(Headers.Uint16).buffer ++ bytes.padLeft(2)
      else if (bytes.size <= 4)
        ByteVector(Headers.Uint32).buffer ++ bytes.padLeft(4)
      else
        ByteVector(Headers.Uint64).buffer ++ bytes.padLeft(8)

    case MsgpackItem.SignedInt(bytes) =>
      if (bytes.size <= 1)
        // positive fixint or negative fixint
        if ((bytes & hex"7f") == bytes || (bytes & hex"c0") == hex"c0")
          bytes.padLeft(1)
        else
          ByteVector(Headers.Int8).buffer ++ bytes.padLeft(1)
      else if (bytes.size <= 2)
        ByteVector(Headers.Int16).buffer ++ bytes.padLeft(2)
      else if (bytes.size <= 4)
        ByteVector(Headers.Int32).buffer ++ bytes.padLeft(4)
      else
        ByteVector(Headers.Int64).buffer ++ bytes.padLeft(8)

    case MsgpackItem.Float32(float) =>
      ByteVector(Headers.Float32).buffer ++ ByteVector.fromInt(java.lang.Float.floatToIntBits(float))

    case MsgpackItem.Float64(double) =>
      ByteVector(Headers.Float64).buffer ++ ByteVector.fromLong(java.lang.Double.doubleToLongBits(double))

    case MsgpackItem.Str(bytes) =>
      if (bytes.size <= 31) {
        ByteVector(0xa0 | bytes.size).buffer ++ bytes
      } else if (bytes.size <= Math.pow(2, 8) - 1) {
        ByteVector(Headers.Str8).buffer ++ ByteVector(bytes.size) ++ bytes
      } else if (bytes.size <= Math.pow(2, 16) - 1) {
        val size = ByteVector.fromShort(bytes.size.toShort)
        ByteVector(Headers.Str16).buffer ++ size ++ bytes
      } else {
        val size = ByteVector.fromInt(bytes.size.toInt)
        ByteVector(Headers.Str32).buffer ++ size ++ bytes
      }

    case MsgpackItem.Bin(bytes) =>
      if (bytes.size <= Math.pow(2, 8) - 1) {
        ByteVector(Headers.Bin8).buffer ++ ByteVector(bytes.size) ++ bytes
      } else if (bytes.size <= Math.pow(2, 16) - 1) {
        val size = ByteVector.fromShort(bytes.size.toShort)
        ByteVector(Headers.Bin16).buffer ++ size ++ bytes
      } else {
        val size = ByteVector.fromInt(bytes.size.toInt).padLeft(4)
        ByteVector(Headers.Bin32).buffer ++ size ++ bytes
      }

    case MsgpackItem.Array(size) =>
      if (size <= 15)
        ByteVector(0x90 | size)
      else if (size <= Math.pow(2, 16) - 1)
        ByteVector(Headers.Array16).buffer ++ ByteVector(size).padLeft(2)
      else
        ByteVector(Headers.Array32).buffer ++ ByteVector(size).padLeft(4)

    case MsgpackItem.Map(size) =>
      if (size <= 15)
        ByteVector(0x80 | size)
      else if (size <= Math.pow(2, 16) - 1)
        ByteVector(Headers.Map16).buffer ++ ByteVector(size).padLeft(2)
      else
        ByteVector(Headers.Map32).buffer ++ ByteVector(size).padLeft(4)

    case MsgpackItem.Extension(tpe, bytes) =>
      if (bytes.size <= 1)
        (ByteVector(Headers.FixExt1).buffer :+ tpe) ++ bytes.padLeft(1)
      else if (bytes.size <= 2)
        (ByteVector(Headers.FixExt2).buffer :+ tpe) ++ bytes.padLeft(2)
      else if (bytes.size <= 4)
        (ByteVector(Headers.FixExt4).buffer :+ tpe) ++ bytes.padLeft(4)
      else if (bytes.size <= 8)
        (ByteVector(Headers.FixExt8).buffer :+ tpe) ++ bytes.padLeft(8)
      else if (bytes.size <= 16)
        (ByteVector(Headers.FixExt16).buffer :+ tpe) ++ bytes.padLeft(16)
      else if (bytes.size <= Math.pow(2, 8) - 1)
        (ByteVector(Headers.Ext8).buffer ++ ByteVector(bytes.size) :+ tpe) ++ bytes
      else if (bytes.size <= Math.pow(2, 16) - 1)
        (ByteVector(Headers.Ext16).buffer ++ ByteVector(bytes.size) :+ tpe) ++ bytes.padLeft(2)
      else
        (ByteVector(Headers.Ext32).buffer ++ ByteVector(bytes.size) :+ tpe) ++ bytes.padLeft(4)

    case MsgpackItem.Timestamp32(seconds) =>
      (ByteVector(Headers.FixExt4).buffer :+ Headers.Timestamp.toByte) ++ ByteVector.fromInt(seconds)

    case MsgpackItem.Timestamp64(combined) =>
      (ByteVector(Headers.FixExt8).buffer :+ Headers.Timestamp.toByte) ++ ByteVector.fromLong(combined)

    case MsgpackItem.Timestamp96(nanoseconds, seconds) =>
      val ns = ByteVector.fromInt(nanoseconds)
      val s = ByteVector.fromLong(seconds)
      (ByteVector(Headers.Ext8).buffer :+ Headers.Timestamp.toByte) ++ ns ++ s

    case MsgpackItem.Nil =>
      ByteVector(Headers.Nil)

    case MsgpackItem.False =>
      ByteVector(Headers.False)

    case MsgpackItem.True =>
      ByteVector(Headers.True)
  }

  def fast: MsgpackItem => ByteVector = {
    case item: MsgpackItem.UnsignedInt =>
      ByteVector(Headers.Uint64) ++ item.bytes.padLeft(8)

    case item: MsgpackItem.SignedInt =>
      ByteVector(Headers.Int64) ++ item.bytes.padLeft(8)

    case item: MsgpackItem.Float32 =>
      ByteVector.fromInt(java.lang.Float.floatToIntBits(item.v))

    case item: MsgpackItem.Float64 =>
      ByteVector.fromLong(java.lang.Double.doubleToLongBits(item.v))

    case item: MsgpackItem.Str =>
      val size = ByteVector.fromInt(item.bytes.size.toInt)
      ByteVector(Headers.Str32) ++ size ++ item.bytes

    case item: MsgpackItem.Bin =>
      val size = ByteVector.fromInt(item.bytes.size.toInt)
      ByteVector(Headers.Bin32) ++ size ++ item.bytes

    case item: MsgpackItem.Array =>
      ByteVector(Headers.Array32) ++ ByteVector.fromInt(item.size).padLeft(4)

    case item: MsgpackItem.Map =>
      ByteVector(Headers.Map32) ++ ByteVector.fromInt(item.size).padLeft(4)

    case item: MsgpackItem.Extension =>
      val size = ByteVector.fromInt(item.bytes.size.toInt)
      val t = ByteVector(item.tpe)
      ByteVector(Headers.Ext32) ++ size ++ t ++ item.bytes

    case item: MsgpackItem.Timestamp32 =>
      ByteVector(Headers.FixExt4) ++ hex"ff" ++ ByteVector.fromInt(item.seconds)

    case item: MsgpackItem.Timestamp64 =>
      ByteVector(Headers.FixExt8) ++ hex"ff" ++ ByteVector.fromLong(item.combined)

    case item: MsgpackItem.Timestamp96 =>
      val ns = ByteVector.fromInt(item.nanoseconds)
      val s = ByteVector.fromLong(item.seconds)
      ByteVector(Headers.Ext8) ++ hex"0c" ++ hex"ff" ++ ns ++ s

    case MsgpackItem.Nil =>
      ByteVector(Headers.Nil)

    case MsgpackItem.False =>
      ByteVector(Headers.False)

    case MsgpackItem.True =>
      ByteVector(Headers.True)
  }
}
