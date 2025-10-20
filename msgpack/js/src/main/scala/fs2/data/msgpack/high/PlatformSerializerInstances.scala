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
package data.msgpack.high
package internal

import fs2.data.msgpack.low.MsgpackItem

import scala.scalajs.js

private[high] trait PlatformSerializerInstances {
  implicit val dateSerializer: MsgpackSerializer[js.Date] = { (date: js.Date) =>
    val milis: Long = date.getTime().toLong // NaN is casted to 0
    val seconds: Long = milis / 1000
    val nano: Int = (milis % 1000).toInt * 1000000

    val secondsLow32: Long = seconds & 0xffffffff
    val secondsLow34: Long = seconds & 0x3ffffffffL
    val nanoLow30: Int = nano & 0x3fffffff

    val item: MsgpackItem =
      if (nano == 0 && (secondsLow32 == seconds)) {
        MsgpackItem.Timestamp32(seconds.toInt)
      } else if (nanoLow30 == nano && secondsLow34 == seconds) {
        val combined = (nanoLow30.toLong << 34) | secondsLow34
        MsgpackItem.Timestamp64(combined)
      } else {
        MsgpackItem.Timestamp96(nano, seconds)
      }

    right1(item)
  }
}
