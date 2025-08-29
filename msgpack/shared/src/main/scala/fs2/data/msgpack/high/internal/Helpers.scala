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
package internal

import scodec.bits.ByteVector
import fs2.data.msgpack.low.MsgpackItem
import fs2.data.msgpack.high.DeserializationResult._

private[high] object Helpers {
  def typeMismatch(got: String, expected: String) = Err(s"Type mismatch: got $got, expected $expected")

  @inline
  def getItem[A](f: (MsgpackItem, Chunk[MsgpackItem]) => DeserializationResult[A]) = new MsgpackDeserializer[A] {
    def deserialize(items: Chunk[MsgpackItem]): DeserializationResult[A] = {
      items.head match {
        case Some(head) => f(head, items.drop(1))
        case None       => NeedsMoreItems(None)
      }
    }
  }

  @inline def firstBitPositive(bytes: ByteVector) = (bytes.head & 0x80) == 0x80
}
