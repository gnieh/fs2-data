package fs2
package data
package msgpack.high

trait MsgpackSerializer[A] {
  def apply(x: A): SerializationResult
}

object MsgpackSerializer {
  def apply[A](x: A)(implicit ev: MsgpackSerializer[A]) = ev(x)
}
