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
