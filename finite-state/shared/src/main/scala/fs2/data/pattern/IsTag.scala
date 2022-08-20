package fs2.data.pattern

import cats._

import scala.annotation.tailrec

trait IsTag[Tag] extends Eq[Tag] {

  /** Indicates whether this tag is open.
    * For instance, tags representing integers are open, strings as well.
    */
  def isOpen: Boolean

  /** The range of this tag type, i.e. all possible values.
    * If the tag is open, returns an empty iterator.
    */
  def range(tag: Tag): Iterator[Tag]

  def subtags(tag: Tag): List[Iterator[Tag]]

  def arity(tag: Tag): Int =
    subtags(tag).size

  @tailrec
  final def hasUnmatched(skel: Skeleton[Tag], matched: Set[Tag]): Boolean =
    skel match {
      case Skeleton.Constructor(tag, _) => isOpen || range(tag).exists(!matched.contains(_))
      case Skeleton.Wildcard(_)         => false
      case Skeleton.As(inner, _)        => hasUnmatched(inner, matched)
    }

}
