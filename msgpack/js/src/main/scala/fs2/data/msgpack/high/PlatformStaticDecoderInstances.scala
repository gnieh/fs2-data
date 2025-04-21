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
          val ms = item.nanoseconds.toDouble / 1_000_000
          d.setUTCSeconds(item.seconds.toDouble)
          d.setUTCMilliseconds(ms)
          ctx.proceed(d)

        case MsgpackItem.Timestamp96(nanoseconds, seconds) =>
          val d = new js.Date()
          val ms = nanoseconds / 1_000_000
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
