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

abstract class MsgpackException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

case class MsgpackMalformedItemException(msg: String, position: Option[Long] = None, inner: Throwable = null)
    extends MsgpackException(position.fold(msg)(pos => s"at position $pos"), inner)

case class MsgpackUnexpectedEndOfStreamException(position: Option[Long] = None, inner: Throwable = null)
    extends MsgpackException(
      position.fold("Unexpected end of stream")(pos => s"Unexpected end of stream starting at position $pos"),
      inner)

case class MsgpackMalformedByteStreamException(msg: String, inner: Throwable = null)
    extends MsgpackException(msg, inner)

case class MsgpackDeserializerException(msg: String, inner: Throwable = null) extends MsgpackException(msg, inner)
case class MsgpackSerializerException(msg: String, inner: Throwable = null) extends MsgpackException(msg, inner)
