package fs2.data.pattern

import scala.annotation.tailrec

final case class VMatrix[Tag, Pat, Out](columns: List[Col[Tag]],
                                        rebuild: List[List[Skeleton[Tag]] => Row[Tag, Pat, Out]]) {

  def horizontalView: Matrix[Tag, Pat, Out] =
    rebuild
      .zip(columns.map(_.patterns).transpose)
      .map { case (f, ps) => f(ps) }

}

final case class Row[Tag, Pat, Out](origin: Pat,
                                    bindings: List[Binding[Select[Tag]]],
                                    patterns: List[Skeleton[Tag]],
                                    output: Out) {

  def bind(binding: Binding[Select[Tag]]): Row[Tag, Pat, Out] =
    copy(bindings = binding :: bindings)

  def isWidcard: Boolean =
    patterns.forall(_.isWildcard)

}

final case class Col[Tag](patterns: List[Skeleton[Tag]]) {

  def constructors: Map[Tag, List[Skeleton[Tag]]] = {
    @tailrec
    def cons(skel: Skeleton[Tag], sig: Map[Tag, List[Skeleton[Tag]]]): Map[Tag, List[Skeleton[Tag]]] =
      skel match {
        case Skeleton.Constructor(tag, args) => sig.updated(tag, args)
        case Skeleton.Wildcard(_)         => sig
        case Skeleton.As(inner, _)           => cons(inner, sig)
      }

    patterns.foldRight(Map.empty[Tag, List[Skeleton[Tag]]])(cons(_, _))
  }

}
