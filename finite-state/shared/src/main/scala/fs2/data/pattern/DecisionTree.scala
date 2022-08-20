package fs2.data.pattern

import fs2.data.matching.Table
import scala.annotation.tailrec

sealed trait DecisionTree[Tag, Out]
object DecisionTree {
  case class Fail[Tag, Out]() extends DecisionTree[Tag, Out]
  case class Leaf[Tag, Out](bindings: List[Binding[Select[Tag]]], out: Out) extends DecisionTree[Tag, Out]
  case class Switch[Tag, Out](on: Select[Tag], branches: Map[Tag, DecisionTree[Tag, Out]], catchAll: Option[DecisionTree[Tag, Out]])
      extends DecisionTree[Tag, Out]

  implicit def DecisionTreeIsTable[I, Tag, O](implicit I: Matchable[I, Tag]): Table.Aux[DecisionTree[Tag, O], I, O] =
    new Table[DecisionTree[Tag, O]] {
      type In = I
      type Out = O

      override def get(tree: DecisionTree[Tag, O])(in: I): Option[O] = {
        @tailrec
        def loop(tags: List[Tag], tree: DecisionTree[Tag, O]): Option[O] =
          (tags, tree) match {
            case (Nil, Leaf(bindings, out)) => Some(out)
            case (tag :: tags, Switch(on, branches, fallBack)) =>
              branches.get(tag) match {
                case Some(tree) => loop(tags, tree)
                case None =>
                  fallBack match {
                    case Some(tree) => loop(tags, tree)
                    case None       => None
                  }
              }
            case (_, _) => None
          }
        loop(I.decompose(in), tree)
      }

    }
}
