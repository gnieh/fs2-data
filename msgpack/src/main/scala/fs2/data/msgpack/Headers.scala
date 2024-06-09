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
  final val Bin8 = 0xc4
  final val Bin16 = 0xc5
  final val Bin32 = 0xc6

  final val Ext8 = 0xc7
  final val Ext16 = 0xc8
  final val Ext32 = 0xc9

  final val Float32 = 0xca
  final val Float64 = 0xcb

  final val Uint8 = 0xcc
  final val Uint16 = 0xcd
  final val Uint32 = 0xce
  final val Uint64 = 0xcf

  final val Int8 = 0xd0
  final val Int16 = 0xd1
  final val Int32 = 0xd2
  final val Int64 = 0xd3

  final val FixExt1 = 0xd4
  final val FixExt2 = 0xd5
  final val FixExt4 = 0xd6
  final val FixExt8 = 0xd7
  final val FixExt16 = 0xd8

  final val Str8 = 0xd9
  final val Str16 = 0xda
  final val Str32 = 0xdb

  final val Array16 = 0xdc
  final val Array32 = 0xdd

  final val Map16 = 0xde
  final val Map32 = 0xdf

  final val Nil = 0xc0
  final val NeverUsed = 0xc1
  final val False = 0xc2
  final val True = 0xc3

  final val Timestamp = -1
}
