package fs2.data.pattern

import scala.annotation.tailrec

/** A [[Selector]] represents the part of the matched input that is
  * under scrutinee during the pattern match.
  */
sealed trait Selector[+Tag] {
  def tags: List[(Tag, Int)] = {
    @tailrec
    def loop(sel: Selector[Tag], acc: List[(Tag, Int)]): List[(Tag, Int)] =
      sel match {
        case Selector.Root                  => acc
        case Selector.Sel(parent, tag, arg) => loop(parent, (tag, arg) :: acc)
      }
    loop(this, Nil)
  }
}
object Selector {
  case object Root extends Selector[Nothing]
  case class Sel[Tag](sel: Selector[Tag], tag: Tag, n: Int) extends Selector[Tag]
}
