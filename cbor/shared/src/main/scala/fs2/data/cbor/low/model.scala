/*
 * Copyright 2020 Lucas Satabin
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
package cbor
package low

import scodec.bits.BitVector

/** A low-level CBOR item. Items are emitted as soon as they are complete
  * They can be used for reading/writing streamed CBOR data, including
  * indefinite length arrays, maps, or strings.
  */
sealed trait CborItem

object CborItem {

  case class PositiveInt(bytes: BitVector) extends CborItem
  case class NegativeInt(bytes: BitVector) extends CborItem

  case class ByteString(bytes: BitVector) extends CborItem
  case object StartIndefiniteByteString extends CborItem

  case class TextString(string: String) extends CborItem
  case object StartIndefiniteTextString extends CborItem

  case class StartArray(size: Long) extends CborItem
  case object StartIndefiniteArray extends CborItem

  case class StartMap(size: Long) extends CborItem
  case object StartIndefiniteMap extends CborItem

  case class Tag(tag: Long) extends CborItem

  case object True extends CborItem
  case object False extends CborItem
  case object Null extends CborItem
  case object Undefined extends CborItem
  case class SimpleValue(value: Byte) extends CborItem

  case class Float16(raw: BitVector) extends CborItem
  case class Float32(raw: BitVector) extends CborItem
  case class Float64(raw: BitVector) extends CborItem

  case object Break extends CborItem

}
