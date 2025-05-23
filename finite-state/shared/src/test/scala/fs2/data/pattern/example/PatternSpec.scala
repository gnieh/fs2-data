/*
 * Copyright 2024 fs2-data Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fs2
package data
package pattern
package example

import cats.effect.IO
import weaver.*

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

sealed trait Guard

sealed trait Pattern
object Pattern {
  case class IntPat(v: Int) extends Pattern
  case class NilPat(tpe: Typ) extends Pattern
  case class ConsPat(tpe: Typ, head: Pattern, tail: Pattern) extends Pattern
  case class OrPat(left: Pattern, right: Pattern) extends Pattern
  case class WildPat(tpe: Typ) extends Pattern

  implicit object PatIsPattern extends IsPattern[Pattern, Guard, Tag] {

    override def trueTag: Tag = Tag.True

    override def decompose(pat: Pattern): List[RawSkeleton[Guard, Tag]] =
      pat match {
        case IntPat(v) =>
          List(RawSkeleton.Constructor(Tag.IntTag(v), Nil, None))
        case NilPat(tpe) =>
          List(RawSkeleton.Constructor(Tag.NilTag(tpe), Nil, None))
        case ConsPat(tpe, head, tail) =>
          decompose(tail).flatMap { tail =>
            decompose(head).map { head =>
              RawSkeleton.Constructor(Tag.ConstTag(tpe), List(head, tail), None)
            }
          }
        case OrPat(left, right) =>
          decompose(left) ++ decompose(right)
        case WildPat(_) =>
          List(RawSkeleton.wildcard)
      }

  }
}

sealed trait Tag
object Tag {
  case class NilTag(tpe: Typ) extends Tag
  case class ConstTag(tpe: Typ) extends Tag
  case class IntTag(v: Int) extends Tag
  case object True extends Tag

  implicit object TagIsTag extends IsTag[Tag] {

    def isOpen(tag: Tag) =
      tag match {
        case NilTag(_) | ConstTag(_) => false
        case _                       => true
      }

    override def eqv(x: Tag, y: Tag): Boolean =
      x == y

    override def range(tag: Tag): Iterator[Tag] =
      tag match {
        case NilTag(tpe)   => Iterator(NilTag(tpe), ConstTag(tpe))
        case ConstTag(tpe) => Iterator(NilTag(tpe), ConstTag(tpe))
        case _             => Iterator.empty
      }

  }
}

object PatternSpec extends SimpleIOSuite {

  test("int pattern") {
    val cases =
      List(Pattern.OrPat(Pattern.IntPat(0), Pattern.IntPat(1)) -> "Zero or One",
           Pattern.WildPat(Typ.TInt) -> "something else")

    new Compiler[IO, Guard, Tag, Pattern, String]
      .compile(cases)
      .map(expect.same(
        DecisionTree.Switch[Guard, Tag, String](
          Selector.Root(),
          Map(
            Tag.IntTag(0) -> DecisionTree.Leaf[Guard, Tag, String]("Zero or One"),
            Tag.IntTag(1) -> DecisionTree.Leaf[Guard, Tag, String]("Zero or One")
          ),
          Some(DecisionTree.Leaf[Guard, Tag, String]("something else"))
        ),
        _
      ))
  }

}
