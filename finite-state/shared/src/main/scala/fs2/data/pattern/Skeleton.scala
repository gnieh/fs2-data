package fs2.data.pattern

import scala.annotation.tailrec

sealed trait Skeleton[+Tag] {
  @tailrec
  final def isWildcard: Boolean = this match {
    case Skeleton.Wildcard(_)    => true
    case Skeleton.Constructor(_, _) => false
    case Skeleton.As(inner, _)      => inner.isWildcard
  }

}
object Skeleton {
  case class Wildcard(as: Option[String]) extends Skeleton[Nothing]
  case class Constructor[Tag](tag: Tag, args: List[Skeleton[Tag]]) extends Skeleton[Tag]
  case class As[Tag](inner: Skeleton[Tag], as: String) extends Skeleton[Tag]
}
