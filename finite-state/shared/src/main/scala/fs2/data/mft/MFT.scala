/*
 * Copyright 2022 Lucas Satabin
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
package mft

import esp.{Depth, ESP, Rhs => ERhs, Pattern, PatternDsl, Tag => ETag}

import cats.{Defer, MonadError}
import cats.syntax.all._
import cats.Show

sealed trait Forest
object Forest {
  case object Self extends Forest
  case object First extends Forest
  case object Second extends Forest

  implicit val show: Show[Forest] = Show.show {
    case Self   => "x0"
    case First  => "x1"
    case Second => "x2"
  }
}

sealed trait EventSelector[Guard, InTag]
object EventSelector {
  case class AnyNode[Guard, InTag](guard: Option[Guard]) extends EventSelector[Guard, InTag]
  case class Node[Guard, InTag](tag: InTag, guard: Option[Guard]) extends EventSelector[Guard, InTag]
  case class AnyLeaf[Guard, InTag](guard: Option[Guard]) extends EventSelector[Guard, InTag]
  case class Leaf[Guard, InTag](v: InTag, guard: Option[Guard]) extends EventSelector[Guard, InTag]
  case class Epsilon[Guard, InTag]() extends EventSelector[Guard, InTag]
}

sealed trait Rhs[+OutTag] {
  def ~[OutTag1 >: OutTag](that: Rhs[OutTag1]): Rhs[OutTag1] =
    (this, that) match {
      case (Rhs.Epsilon, _) => that
      case (_, Rhs.Epsilon) => this
      case (_, _)           => Rhs.Concat(this, that)
    }
}
object Rhs {
  case class Call[OutTag](q: Int, x: Forest, parameters: List[Rhs[OutTag]]) extends Rhs[OutTag]
  case object Epsilon extends Rhs[Nothing]
  case class Param(n: Int) extends Rhs[Nothing]
  case class Node[OutTag](tag: OutTag, children: Rhs[OutTag]) extends Rhs[OutTag]
  case class CopyNode[OutTag](children: Rhs[OutTag]) extends Rhs[OutTag]
  case class Leaf[OutTag](value: OutTag) extends Rhs[OutTag]
  case object CopyLeaf extends Rhs[Nothing]
  case class Concat[OutTag](fst: Rhs[OutTag], snd: Rhs[OutTag]) extends Rhs[OutTag]

  implicit def show[O: Show]: Show[Rhs[O]] =
    Show.show {
      case Call(q, x, Nil)     => show"q$q($x)"
      case Call(q, x, ps)      => show"q$q($x${ps.map(_.show).mkString(", ", ", ", "")})"
      case Epsilon             => ""
      case Param(i)            => show"y$i"
      case Node(tag, children) => show"<$tag>($children)"
      case CopyNode(children)  => show"%t($children)"
      case Leaf(value)         => show"<$value>"
      case CopyLeaf            => "%t"
      case Concat(l, r)        => show"$l $r"
    }
}

/** A Macro Forest Transducer, as described in _Streamlining Functional XML Processing_.
  * To each state is associated a collection of rules, matching a forest and
  * generating a new one.
  *
  * An MFT is an intermediate structure towards a compiled [[fs2.data.esp.ESP Events Stream Processor]]
  */
private[data] class MFT[Guard, InTag, OutTag](init: Int, val rules: Map[Int, Rules[Guard, InTag, OutTag]]) {

  /** Compiles this MFT into an ESP.
    * The generated ESP contains one decision tree encoding all the patterns
    * of this MFT.
    */
  def esp[F[_]](implicit F: MonadError[F, Throwable], defer: Defer[F]): F[ESP[F, Guard, InTag, OutTag]] = {

    val dsl = new PatternDsl[Guard, InTag]
    import dsl._

    def translateRhs(rhs: Rhs[OutTag]): ERhs[OutTag] =
      rhs match {
        case Rhs.Call(q, Forest.Self, params)   => ERhs.SelfCall(q, params.map(translateRhs(_)))
        case Rhs.Call(q, Forest.First, params)  => ERhs.Call(q, Depth.Value(0), params.map(translateRhs(_)))
        case Rhs.Call(q, Forest.Second, params) => ERhs.Call(q, Depth.Value(1), params.map(translateRhs(_)))
        case Rhs.Param(i)                       => ERhs.Param(i)
        case Rhs.Epsilon                        => ERhs.Epsilon
        case Rhs.Node(tag, inner)               => ERhs.Tree(tag, translateRhs(inner))
        case Rhs.CopyNode(inner)                => ERhs.CapturedTree("in", translateRhs(inner))
        case Rhs.Leaf(v)                        => ERhs.Leaf(v)
        case Rhs.CopyLeaf                       => ERhs.CapturedLeaf("in")
        case Rhs.Concat(rhs1, rhs2)             => ERhs.Concat(translateRhs(rhs1), translateRhs(rhs2))
      }

    val cases = rules.toList.flatMap { case (q, Rules(params, tree)) =>
      tree.flatMap {
        case (EventSelector.Node(tag, None), rhs) =>
          List(state(q, 0)(open(tag)) -> translateRhs(rhs))
        case (EventSelector.AnyNode(None), rhs) =>
          List(state(q, 0)(open) -> translateRhs(rhs))
        case (EventSelector.Node(tag, Some(guard)), rhs) =>
          List(state(q, 0)(open(tag).when(guard)) -> translateRhs(rhs))
        case (EventSelector.AnyNode(Some(guard)), rhs) =>
          List(state(q, 0)(open.when(guard)) -> translateRhs(rhs))
        case (EventSelector.Leaf(in, None), rhs) =>
          List(state(q, 0)(value(in)) -> translateRhs(rhs))
        case (EventSelector.AnyLeaf(None), rhs) =>
          List(state(q, 0)(value) -> translateRhs(rhs))
        case (EventSelector.Leaf(in, Some(guard)), rhs) =>
          List(state(q, 0)(value(in).when(guard)) -> translateRhs(rhs))
        case (EventSelector.AnyLeaf(Some(guard)), rhs) =>
          List(state(q, 0)(value.when(guard)) -> translateRhs(rhs))
        case (EventSelector.Epsilon(), rhs) =>
          val dflt = translateRhs(rhs)
          List(state(q, 0)(close) -> dflt, state(q)(eos) -> dflt)
      } ++ List(
        state(q)(open) -> ERhs.Call(q, Depth.Increment, params.map(ERhs.Param(_))),
        state(q, 0)(close) -> ERhs.Epsilon,
        state(q)(close) -> ERhs.Call(q, Depth.Decrement, params.map(ERhs.Param(_))),
        state(q)(value) -> ERhs.Call(q, Depth.Copy, params.map(ERhs.Param(_))),
        state(q)(eos) -> ERhs.Epsilon
      )
    }

    val compiler =
      new pattern.Compiler[F, Guard, ETag[InTag], Pattern[Guard, InTag], ERhs[OutTag]]

    compiler.compile(cases).map(new ESP(init, rules.fmap(_.params), _))
  }

}

object MFT {

  implicit def show[G: Show, I: Show, O: Show]: Show[MFT[G, I, O]] = Show.show { mft =>
    mft.rules.toList
      .sortBy(_._1)
      .map { case (src, rules) =>
        val params =
          if (rules.params.isEmpty)
            ""
          else
            rules.params.map(i => s"y$i").mkString(", ", ", ", "")
        implicit val showSelector: Show[EventSelector[G, I]] = Show.show {
          case EventSelector.AnyNode(g) => show"(<%t>$params)${g.fold("")(g => show" when $g")}"
          case EventSelector.Node(t, g) => show"(<$t>$params)${g.fold("")(g => show" when $g")}"
          case EventSelector.AnyLeaf(g) => show"(<%t />$params)${g.fold("")(g => show" when $g")}"
          case EventSelector.Leaf(t, g) => show"(<$t />$params)${g.fold("")(g => show" when $g")}"
          case EventSelector.Epsilon()  => show"(ε$params)"
        }
        rules.tree
          .map { case (pat, rhs) =>
            show"q$src$pat -> $rhs"
          }
          .mkString_("\n")
      }
      .mkString_("\n\n")
  }

}
