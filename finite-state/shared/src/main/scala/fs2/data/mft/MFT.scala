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
import scala.annotation.tailrec
import scala.collection.compat._

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
  case class ApplyToLeaf[OutTag](f: OutTag => Either[String, OutTag]) extends Rhs[OutTag]
  case class Concat[OutTag](fst: Rhs[OutTag], snd: Rhs[OutTag]) extends Rhs[OutTag]

  implicit def show[O: Show]: Show[Rhs[O]] =
    Show.show {
      case Call(q, x, Nil)     => show"q$q($x)"
      case Call(q, x, ps)      => show"q$q($x${(ps: List[Rhs[O]]).mkString_(", ", ", ", "")})"
      case Epsilon             => ""
      case Param(i)            => show"y$i"
      case Node(tag, children) => show"<$tag>($children)"
      case CopyNode(children)  => show"%t($children)"
      case Leaf(value)         => show"<$value>"
      case CopyLeaf            => "%t"
      case ApplyToLeaf(_)      => "<leaf-function>"
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

  /** Returns an MFT that has the same behavior, but only propagates
    * parameters that actually contribute to the output.
    */
  def removeUnusedParameters: MFT[Guard, InTag, OutTag] = {
    def bareOccurences(rhs: Rhs[OutTag]): Set[Int] =
      rhs match {
        case Rhs.Param(i)           => Set(i)
        case Rhs.Node(_, children)  => bareOccurences(children)
        case Rhs.CopyNode(children) => bareOccurences(children)
        case Rhs.Concat(rhs1, rhs2) => bareOccurences(rhs1) ++ bareOccurences(rhs2)
        case _                      => Set()
      }

    def findAllCalls(rhs: Rhs[OutTag]): List[Rhs.Call[OutTag]] =
      rhs match {
        case Rhs.Call(q, x, ps)     => Rhs.Call(q, x, ps) :: ps.flatMap(findAllCalls(_))
        case Rhs.Node(_, children)  => findAllCalls(children)
        case Rhs.CopyNode(children) => findAllCalls(children)
        case Rhs.Concat(fst, snd)   => findAllCalls(fst) ++ findAllCalls(snd)
        case _                      => Nil
      }

    val usedParams =
      rules.fmap { case Rules(_, rhss) =>
        rhss.map { case (_, rhs) => bareOccurences(rhs) }.combineAll
      }

    @tailrec
    def findAllUsedParams(usedParams: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
      val newUsed = usedParams.combine(rules.fmap { case Rules(_, rhss) =>
        rhss.flatMap { case (_, rhs) =>
          findAllCalls(rhs).flatMap { case Rhs.Call(q1, _, args) =>
            val usedInQ1 = usedParams.getOrElse(q1, Set())
            args.zipWithIndex.collect {
              case (rhs, i) if usedInQ1.contains(i) =>
                bareOccurences(rhs)
            }
          }
        }.combineAll
      })
      if (newUsed == usedParams)
        usedParams
      else
        findAllUsedParams(newUsed)
    }

    val allUsedParams = findAllUsedParams(usedParams)

    def dropUnused(rhs: Rhs[OutTag], usedParams: Set[Int]): Rhs[OutTag] =
      rhs match {
        case Rhs.Call(q, x, args) =>
          Rhs.Call(q,
                   x,
                   args.zipWithIndex
                     .collect {
                       case (a, i) if allUsedParams.getOrElse(q, Set()).contains(i) =>
                         dropUnused(a, usedParams)
                     })
        case Rhs.Node(tag, children) => Rhs.Node(tag, dropUnused(children, usedParams))
        case Rhs.CopyNode(children)  => Rhs.CopyNode(dropUnused(children, usedParams))
        case Rhs.Concat(rhs1, rhs2)  => Rhs.Concat(dropUnused(rhs1, usedParams), dropUnused(rhs2, usedParams))
        case Rhs.Param(i)            => Rhs.Param(usedParams.count(_ < i))
        case _                       => rhs
      }

    val rules1 = rules.map2(allUsedParams) { case (Rules(_, rhss), usedParams) =>
      Rules(usedParams.size, rhss.map { case (sel, rhs) => (sel, dropUnused(rhs, usedParams)) })
    }

    new MFT(init, rules1)
  }

  /** Returns an MFT that has the same behavior but with stay moves inlined when possible. */
  def inlineStayMoves: MFT[Guard, InTag, OutTag] = {
    // first we gather all the stay states, for which the RHS is only calling other states on self
    // and is the same for all cases.
    def hasOnlySelfCalls(rhs: Rhs[OutTag]): Boolean =
      rhs match {
        case Rhs.Call(_, Forest.Self, _) => true
        case Rhs.Call(_, _, _)           => false
        case Rhs.Node(_, children)       => hasOnlySelfCalls(children)
        case Rhs.CopyNode(children)      => hasOnlySelfCalls(children)
        case Rhs.Concat(rhs1, rhs2)      => hasOnlySelfCalls(rhs1) && hasOnlySelfCalls(rhs2)
        case _                           => true
      }

    val stayStates = rules.mapFilter { rules =>
      if (rules.isWildcard)
        rules.tree.headOption.collect { case (_, rhs) if hasOnlySelfCalls(rhs) => rhs }
      else
        none
    }

    def subst(rhs: Rhs[OutTag], x: Forest, args: List[Rhs[OutTag]]): Rhs[OutTag] =
      rhs match {
        case Rhs.Call(q, _, args1)  => Rhs.Call(q, x, args1.map(subst(_, x, args)))
        case Rhs.Param(i)           => args.lift(i).getOrElse(Rhs.Epsilon)
        case Rhs.Node(t, children)  => Rhs.Node(t, subst(children, x, args))
        case Rhs.CopyNode(children) => Rhs.CopyNode(subst(children, x, args))
        case Rhs.Concat(rhs1, rhs2) => Rhs.Concat(subst(rhs1, x, args), subst(rhs2, x, args))
        case _                      => rhs
      }

    def inlineStayCalls(rhs: Rhs[OutTag]): Rhs[OutTag] =
      rhs match {
        case Rhs.Call(q, x, args) =>
          stayStates.get(q) match {
            case Some(rhs) =>
              subst(rhs, x, args.map(inlineStayCalls(_)))
            case None => rhs
          }
        case Rhs.Node(t, children)  => Rhs.Node(t, inlineStayCalls(children))
        case Rhs.CopyNode(children) => Rhs.CopyNode(inlineStayCalls(children))
        case Rhs.Concat(rhs1, rhs2) => Rhs.Concat(inlineStayCalls(rhs1), inlineStayCalls(rhs2))
        case _                      => rhs
      }

    val rules1 = rules.fmap { case Rules(nparams, rhss) =>
      Rules(nparams,
            rhss.map { case (sel, rhs) =>
              (sel, inlineStayCalls(rhs))
            })
    }

    new MFT(init, rules1)
  }

  /** Returns an MFT that has the same behavior but without states
    * that are never called from the initial state.
    */
  def removeUnreachableStates: MFT[Guard, InTag, OutTag] = {
    def reachable(toProcess: List[Int], processed: Set[Int]): Set[Int] =
      toProcess match {
        case q :: qs =>
          if (processed.contains(q)) {
            reachable(qs, processed)
          } else {
            def calledStates(rhs: Rhs[OutTag]): List[Int] =
              rhs match {
                case Rhs.Call(q, _, args)   => q :: args.flatMap(calledStates(_))
                case Rhs.Node(_, children)  => calledStates(children)
                case Rhs.CopyNode(children) => calledStates(children)
                case Rhs.Concat(rhs1, rhs2) => calledStates(rhs1) ++ calledStates(rhs2)
                case _                      => Nil
              }
            val newStates = rules.get(q).map(_.tree.map(_._2).flatMap(calledStates(_))).getOrElse(Nil)
            reachable(newStates ++ qs, processed + q)
          }
        case Nil => processed
      }

    val reachableStates = reachable(List(init), Set())

    new MFT(init, rules.view.filterKeys(reachableStates.contains(_)).toMap)
  }

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
        case Rhs.CopyNode(inner)                => ERhs.CapturedTree(translateRhs(inner))
        case Rhs.Leaf(v)                        => ERhs.Leaf(v)
        case Rhs.CopyLeaf                       => ERhs.CapturedLeaf
        case Rhs.ApplyToLeaf(f)                 => ERhs.ApplyToLeaf(f)
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
        state(q)(open) -> ERhs.Call(q, Depth.Increment, List.tabulate(params)(ERhs.Param(_))),
        state(q, 0)(close) -> ERhs.Epsilon,
        state(q)(close) -> ERhs.Call(q, Depth.Decrement, List.tabulate(params)(ERhs.Param(_))),
        state(q)(value) -> ERhs.Call(q, Depth.Copy, List.tabulate(params)(ERhs.Param(_))),
        state(q)(eos) -> ERhs.Epsilon
      )
    }

    val compiler =
      new pattern.Compiler[F, Guard, ETag[InTag], Pattern[Guard, InTag], ERhs[OutTag]]

    compiler.compile(cases).map(new ESP(init, rules.fmap(_.nparams), _))
  }

}

object MFT {

  implicit def show[G: Show, I: Show, O: Show]: Show[MFT[G, I, O]] = Show.show { mft =>
    mft.rules.toList
      .sortBy(_._1)
      .map { case (src, rules) =>
        val params =
          if (rules.nparams == 0)
            ""
          else
            List.tabulate(rules.nparams)(i => s"y$i").mkString(", ", ", ", "")
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
