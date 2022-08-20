package fs2.data.pattern

sealed trait Select[+Tag]
object Select {
  case object NoSel extends Select[Nothing]
  case class Sel[Tag](sel: Select[Tag], tag: Tag, n: Int) extends Select[Tag]
}
