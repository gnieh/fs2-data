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
  case class Float32(v: Float) extends MsgpackItem

  /** Double precision IEE 754 float */
  case class Float64(v: Double) extends MsgpackItem

  /** UTF-8 encoded string */
  case class Str(bytes: ByteVector) extends MsgpackItem

  case class Bin(bytes: ByteVector) extends MsgpackItem
  case class Array(size: Long) extends MsgpackItem
  case class Map(size: Long) extends MsgpackItem

  case class Extension(tpe: Byte, bytes: ByteVector) extends MsgpackItem

  // Predefined extension types
  case class Timestamp32(seconds: Int) extends MsgpackItem

  /** Stores data in a 30-bit [[nanoseconds]] and a 34-bit [[seconds]] fields, both of which are accessible as class
    * attributes. To ensure valid data length at the type level, both fields are constructed from a single 64-bit
    * [[combined]] variable.
    * @param combined [[nanoseconds]] and [[seconds]] combined into a signle 64-bit value
    */
  case class Timestamp64(combined: Long) extends MsgpackItem {
    /* We are sure that (x: Long) >> 34 fits in an int but we also need to add a mask so that we don't end up with
     * a negative number.
     */
    val nanoseconds: Int = (0x000000003fffffffL & (combined >> 34)).toInt
    val seconds: Long = combined & 0x00000003ffffffffL
  }
  case class Timestamp96(nanoseconds: Int, seconds: Long) extends MsgpackItem

  case object Nil extends MsgpackItem
  case object True extends MsgpackItem
  case object False extends MsgpackItem
}
