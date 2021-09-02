/*
 * Copyright 2021 Lucas Satabin
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

package fs2.data.cbor.high

import scodec.bits.ByteVector

import scala.collection.{Map => SMap}

sealed trait CborValue

object CborValue {

  case class Integer(value: BigInt) extends CborValue
  case class ByteString(bytes: ByteVector) extends CborValue
  case class TextString(text: String) extends CborValue
  case class Array(values: List[CborValue], indefinite: Boolean) extends CborValue
  case class Map(values: SMap[CborValue, CborValue], indefinite: Boolean) extends CborValue
  case class Tagged(tag: Long, value: CborValue) extends CborValue
  case object True extends CborValue
  case object False extends CborValue
  case object Null extends CborValue
  case object Undefined extends CborValue
  case class SimpleValue(value: Byte) extends CborValue
  case class Float32(value: Float) extends CborValue
  case class Float64(value: Double) extends CborValue

}
