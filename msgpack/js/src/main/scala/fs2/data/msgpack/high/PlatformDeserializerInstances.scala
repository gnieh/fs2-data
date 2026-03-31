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

import internal.Helpers._
import fs2.data.msgpack.low.MsgpackItem
import DeserializationResult._

import scala.scalajs.js

private[high] trait PlatformDeserializerInstances {
  implicit val dateDeserializer: MsgpackDeserializer[js.Date] = getItem { (item, tail) =>
    item match {
      case MsgpackItem.Timestamp32(seconds) =>
        val ms = seconds.toDouble * 1000
        val d = new js.Date(ms)
        Ok(d, tail)

      case item: MsgpackItem.Timestamp64 =>
        val ms = (item.nanoseconds.toDouble / 1000000) + (item.seconds.toDouble * 1000)
        val d = new js.Date(ms)
        Ok(d, tail)

      case MsgpackItem.Timestamp96(nanoseconds, seconds) =>
        val ms = (nanoseconds.toDouble / 1000000) + (seconds.toDouble * 1000)
        val d = new js.Date(ms)
        Ok(d, tail)

      case x =>
        typeMismatch(s"MsgpackItem.${x.getClass.getSimpleName}", "js.Date")
    }
  }
}
