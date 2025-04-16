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
package high

import scodec.bits.ByteVector

sealed trait MsgpackValue

object MsgpackValue {
  case class Integer(x: scala.Long) extends MsgpackValue
  case class String(x: java.lang.String) extends MsgpackValue
  case class Array[A](x: scala.List[A]) extends MsgpackValue
  case class Bin(x: ByteVector) extends MsgpackValue
  case class Map[K, V](x: scala.collection.Map[K, V]) extends MsgpackValue
  case class Boolean(x: scala.Boolean) extends MsgpackValue
  case class Float(x: scala.Float) extends MsgpackValue
  case class Double(x: scala.Double) extends MsgpackValue

  case class Timestamp(x: java.time.Instant) extends MsgpackValue
  case class Extension(tpe: Byte, bytes: ByteVector) extends MsgpackValue

  case object Nil extends MsgpackValue
}
