package fs2
package data
package pattern

import cats.effect._
import cats.syntax.all._

import weaver._

sealed trait Typ {
  val range: Iterator[Tag] =
    this match {
      case Typ.TInt       => Iterator.range(Int.MinValue, Int.MaxValue).map(Tag.IntTag(_))
      case Typ.TList(elt) => Iterator(Tag.NilTag(elt), Tag.ConstTag(elt))
    }
}
object Typ {
  case object TInt extends Typ
  case class TList(elt: Typ) extends Typ
}

sealed trait Pat
object Pat {
  case class VarPat(tpe: Typ, name: String) extends Pat
  case class IntPat(v: Int) extends Pat
  case class NilPat(tpe: Typ) extends Pat
  case class ConsPat(tpe: Typ, head: Pat, tail: Pat) extends Pat
  case class OrPat(left: Pat, right: Pat) extends Pat
  case class AsPat(inner: Pat, name: String) extends Pat
  case class WildPat(tpe: Typ) extends Pat

  implicit object PatIsPattern extends Pattern[Pat, Tag] {

    override def decompose(pat: Pat): List[Skeleton[Tag]] =
      pat match {
        case VarPat(tpe, name) =>
          List(Skeleton.Wildcard(name.some))
        case IntPat(v) =>
          List(Skeleton.Constructor(Tag.IntTag(v), Nil))
        case NilPat(tpe) =>
          List(Skeleton.Constructor(Tag.NilTag(tpe), Nil))
        case ConsPat(tpe, head, tail) =>
          decompose(tail).flatMap { tail =>
            decompose(head).map { head =>
              Skeleton.Constructor(Tag.ConstTag(tpe), List(head, tail))
            }
          }
        case OrPat(left, right) =>
          decompose(left) ++ decompose(right)
        case AsPat(inner, name) =>
          decompose(inner).map(Skeleton.As(_, name))
        case WildPat(tpe) =>
          List(Skeleton.Wildcard(none))
      }

  }
}

sealed trait Tag
object Tag {
  case class NilTag(tpe: Typ) extends Tag
  case class ConstTag(tpe: Typ) extends Tag
  case class IntTag(v: Int) extends Tag

  implicit object TagIsTag extends IsTag[Tag] {

    def isOpen = true

    override def eqv(x: Tag, y: Tag): Boolean =
      x == y

    override def range(tag: Tag): Iterator[Tag] =
      Iterator.empty

    override def subtags(tag: Tag): List[Iterator[Tag]] =
      tag match {
        case NilTag(_)     => Nil
        case ConstTag(elt) => List(elt.range, Typ.TList(elt).range)
        case IntTag(_)     => Nil
      }

  }
}

object PatternSpec extends SimpleIOSuite {

  test("int pattern") {
    val cases =
      List(Pat.OrPat(Pat.IntPat(0), Pat.IntPat(1)) -> "Zero or One", Pat.WildPat(Typ.TInt) -> "something else")

    new Compiler[IO, Tag, Pat, String](Heuristic.firstRow)
      .compile(cases)
      .map(expect.same(
        DecisionTree.Switch[Tag, String](
          Select.NoSel,
          Map(
            Tag.IntTag(0) -> DecisionTree.Leaf[Tag, String](Nil, "Zero or One"),
            Tag.IntTag(1) -> DecisionTree.Leaf[Tag, String](Nil, "Zero or One")
          ),
          Some(DecisionTree.Leaf[Tag, String](List((None, Select.NoSel)), "something else"))
        ),
        _
      ))
  }

}
