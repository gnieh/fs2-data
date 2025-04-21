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

import java.time.Instant
import internal.Helpers._
import fs2.data.msgpack.low._
import fs2.data.msgpack.MsgpackDecodingTypeMismatchException

private[high] trait PlatformStaticDecoderInstances {
  implicit object instantDecoder extends MsgpackDecoder[Instant] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Timestamp32(seconds) =>
          val instant = java.time.Instant.ofEpochSecond(seconds.toLong)
          ctx.proceed(instant)

        case item: MsgpackItem.Timestamp64 =>
          val instant = java.time.Instant.ofEpochSecond(item.seconds, item.nanoseconds.toLong)
          ctx.proceed(instant)

        case MsgpackItem.Timestamp96(nanoseconds, seconds) =>
          val instant = java.time.Instant.ofEpochSecond(seconds, nanoseconds.toLong)
          ctx.proceed(instant)

        case x =>
          Pull.raiseError(
            new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Instant"))
      }
    }
  }
}
