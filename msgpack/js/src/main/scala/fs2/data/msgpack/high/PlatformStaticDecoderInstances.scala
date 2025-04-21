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
import fs2.data.msgpack.MsgpackDecodingTypeMismatchException
import fs2.data.msgpack.low.MsgpackItem

import scala.scalajs.js

private[high] trait PlatformStaticDecoderInstances {
  implicit object dateDecoder extends MsgpackDecoder[js.Date] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Timestamp32(seconds) =>
          val d = new js.Date()
          d.setUTCSeconds(seconds.toDouble)
          ctx.proceed(d)

        case item: MsgpackItem.Timestamp64 =>
          val d = new js.Date()
          val ms = item.nanoseconds.toDouble / 1000000
          d.setUTCSeconds(item.seconds.toDouble)
          d.setUTCMilliseconds(ms)
          ctx.proceed(d)

        case MsgpackItem.Timestamp96(nanoseconds, seconds) =>
          val d = new js.Date()
          val ms = nanoseconds / 1000000
          d.setUTCSeconds(seconds.toDouble)
          d.setUTCMilliseconds(ms.toDouble)
          ctx.proceed(d)

        case x =>
          Pull.raiseError(
            new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "js.Date"))
      }
    }
  }
}
