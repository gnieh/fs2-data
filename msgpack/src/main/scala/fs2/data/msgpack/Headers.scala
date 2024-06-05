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

package fs2.data.msgpack

object Headers {
  final val Bin8 = 0xc4.toByte
  final val Bin16 = 0xc5.toByte
  final val Bin32 = 0xc6.toByte

  final val Ext8 = 0xc7.toByte
  final val Ext16 = 0xc8.toByte
  final val Ext32 = 0xc9.toByte

  final val Float32 = 0xca.toByte
  final val Float64 = 0xcb.toByte

  final val Uint8 = 0xcc.toByte
  final val Uint16 = 0xcd.toByte
  final val Uint32 = 0xce.toByte
  final val Uint64 = 0xcf.toByte

  final val Int8 = 0xd0.toByte
  final val Int16 = 0xd1.toByte
  final val Int32 = 0xd2.toByte
  final val Int64 = 0xd3.toByte

  final val FixExt1 = 0xd4.toByte
  final val FixExt2 = 0xd5.toByte
  final val FixExt4 = 0xd6.toByte
  final val FixExt8 = 0xd7.toByte
  final val FixExt16 = 0xd8.toByte

  final val Str8 = 0xd9.toByte
  final val Str16 = 0xda.toByte
  final val Str32 = 0xdb.toByte

  final val Array16 = 0xdc.toByte
  final val Array32 = 0xdd.toByte

  final val Map16 = 0xde.toByte
  final val Map32 = 0xdf.toByte

  final val Nil = 0xc0.toByte
  final val NeverUsed: Byte = 0xc1.toByte
  final val False = 0xc2.toByte
  final val True = 0xc3.toByte

  final val Timestamp = 0xff.toByte
}
