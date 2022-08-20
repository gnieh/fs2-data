package fs2.data

import cats.syntax.all._

import scala.annotation.tailrec

package object pattern {

  type Matrix[Tag, Pat, Out] = List[Row[Tag, Pat, Out]]

  implicit class MatrixOps[Tag, Pat, Out](val m: Matrix[Tag, Pat, Out]) extends AnyVal {

    def verticalView: VMatrix[Tag, Pat, Out] =
      VMatrix(m.map(_.patterns).transpose.map(Col(_)),
              m.map({ case Row(pat, bindings, _, out) => patterns => Row(pat, bindings, patterns, out) }))

  }

  type Binding[Expr] = (Option[String], Expr)

  private[pattern] def headConstructors[Tag](skels: List[Skeleton[Tag]]): List[Skeleton.Constructor[Tag]] = {
    @tailrec
    def go(skel: Skeleton[Tag]): Option[Skeleton.Constructor[Tag]] =
      skel match {
        case Skeleton.Constructor(tag, args) => Skeleton.Constructor(tag, args).some
        case Skeleton.Wildcard(_)            => none
        case Skeleton.As(p, _)               => go(p)
      }
    skels.flatMap(go(_))
  }

}
