package fs2.data.esp

import fs2.data.pattern.IsTag

sealed trait Tag[+T]
object Tag {
  case object Input extends Tag[Nothing]
  case class State(q: Int) extends Tag[Nothing]
  case class Depth(d: Int) extends Tag[Nothing]
  case class Name(name: String) extends Tag[Nothing]
  case object Open extends Tag[Nothing]
  case object Close extends Tag[Nothing]
  case object End extends Tag[Nothing]
  case class Value[T](v: T) extends Tag[T]

  implicit def TagIsTag[T]: IsTag[Tag[T]] = new IsTag[Tag[T]] {

    def isOpen(tag: Tag[T]) =
      tag match {
        case Input | Open | Close | End => false
        case _                          => true
      }

    override def eqv(x: Tag[T], y: Tag[T]): Boolean =
      x == y

    override def range(tag: Tag[T]): Iterator[Tag[T]] =
      tag match {
        case Input => Iterator(Input)
        case Open  => Iterator(Open)
        case Close => Iterator(Close)
        case End   => Iterator(End)
        case _     => Iterator.empty
      }

  }

}
