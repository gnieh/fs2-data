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

package fs2.data.msgpack.low

import scodec.bits.ByteVector

sealed trait MsgpackItem

object MsgpackItem {
  case class UnsignedInt(bytes: ByteVector) extends MsgpackItem
  case class SignedInt(bytes: ByteVector) extends MsgpackItem

  /** Single precision IEE 754 float */
  case class Float32(bytes: ByteVector) extends MsgpackItem

  /** Double precision IEE 754 float */
  case class Float64(bytes: ByteVector) extends MsgpackItem

  /** UTF-8 encoded string */
  case class Str(bytes: ByteVector) extends MsgpackItem

  case class Bin(bytes: ByteVector) extends MsgpackItem
  case class Array(size: Int) extends MsgpackItem
  case class Map(size: Int) extends MsgpackItem

  case class Extension(tpe: Byte, bytes: ByteVector) extends MsgpackItem

  // Predefined extension types
  case class Timestamp32(seconds: ByteVector) extends MsgpackItem
  case class Timestamp64(nanoseconds: ByteVector, seconds: ByteVector) extends MsgpackItem
  case class Timestamp96(nanoseconds: ByteVector, seconds: ByteVector) extends MsgpackItem

  case object Nil extends MsgpackItem
  case object True extends MsgpackItem
  case object False extends MsgpackItem
}
