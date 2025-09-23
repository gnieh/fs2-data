package fs2
package data
package msgpack.high

import fs2.data.msgpack.low.MsgpackItem

trait MsgpackSerializer[A] {
  def apply(x: A): Either[String, Chunk[MsgpackItem]]
}

object MsgpackSerializer {
  def apply[A](x: A)(implicit ev: MsgpackSerializer[A]) = ev(x)
}
