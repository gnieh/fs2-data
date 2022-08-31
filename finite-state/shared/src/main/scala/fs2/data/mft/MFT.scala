/*
 * Copyright 2019-2022 Lucas Satabin
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

import esp.{Depth, ESP, Rhs => ERhs, Pattern, Tag => ETag}
import pattern._

import cats.effect._
import cats.syntax.all._
import fs2.data.esp.PatternDsl

sealed trait Forest {
  def fold[T](children: => T)(siblings: => T): T =
    this match {
      case Forest.Children => children
      case Forest.Siblings => siblings
    }
}
object Forest {
  case object Children extends Forest
  case object Siblings extends Forest
}

sealed trait EventSelector[+Tag, +In]
object EventSelector {
  case class Node[Tag](tag: Tag) extends EventSelector[Tag, Nothing]
  case class Value[In](v: In) extends EventSelector[Nothing, In]
  case object Epsilon extends EventSelector[Nothing, Nothing]
}

sealed trait Rhs[+Tag, +Out]
object Rhs {
  case class Call[Tag, Out](q: Int, x: Forest, parameters: List[Rhs[Tag, Out]]) extends Rhs[Tag, Out]
  case object Epsilon extends Rhs[Nothing, Nothing]
  case class Param(n: Int) extends Rhs[Nothing, Nothing]
  case class Node[Tag, Out](tag: Tag, children: Rhs[Tag, Out]) extends Rhs[Tag, Out]
  case class Leaf[Out](value: Out) extends Rhs[Nothing, Out]
  case class Concat[Tag, Out](fst: Rhs[Tag, Out], snd: Rhs[Tag, Out]) extends Rhs[Tag, Out]
}

case class Rules[Tag, In, Out](params: List[Int], tree: List[(EventSelector[Tag, In], Rhs[Tag, Out])])

trait Conversion[Tag, Out] {
  def makeName(t: Tag): String
  def makeOpenOut(t: Tag): Out
  def makeCloseOut(t: Tag): Out
}

/** A Macro Forest Transducer, as described in _Streamlining Functional XML Processing_.
  * To each state is associated a collection of rules, matching a forest and
  * generating a new one.
  *
  * An MFT is an intermediate structure towards a compiled [[fs2.data.esp.ESP Events Stream Processor]]
  */
private[data] class MFT[Tag, In, Out](init: Int, rules: Map[Int, Rules[Tag, In, Out]]) {

  /** Compiles this MFT into an ESP.
    * The generated ESP contains one decision tree encoding all the patterns
    * of this MFT.
    */
  def esp[F[_]](implicit
      F: Sync[F],
      Tag: Conversion[Tag, Out],
      Sel: Selectable[In, ETag[In]]): F[ESP[F, DecisionTree[ETag[In], ERhs[Out]], In, Out]] = {

    val dsl = new PatternDsl[Tag, In]
    import dsl._

    def translateRhs(rhs: Rhs[Tag, Out]): ERhs[Out] =
      rhs match {
        case Rhs.Call(q, f, params) => ERhs.Call(q, Depth.Value(f.fold(0)(1)), params.map(translateRhs(_)))
        case Rhs.Param(i)           => ERhs.Param(i)
        case Rhs.Epsilon            => ERhs.Epsilon
        case Rhs.Node(tag, inner)   => ERhs.Tree(Tag.makeOpenOut(tag), translateRhs(inner), Tag.makeCloseOut(tag))
        case Rhs.Leaf(v)            => ERhs.Leaf(v)
        case Rhs.Concat(rhs1, rhs2) => ERhs.Concat(translateRhs(rhs1), translateRhs(rhs2))
      }

    val cases = rules.toList.flatMap { case (q, Rules(params, tree)) =>
      tree.flatMap {
        case (EventSelector.Node(tag), rhs) =>
          List(
            state(q, 0)(open(tag)) -> translateRhs(rhs)
          )
        case (EventSelector.Value(in), rhs) =>
          List(state(q, 0)(value(in)) -> translateRhs(rhs))
        case (EventSelector.Epsilon, rhs) =>
          val dflt = translateRhs(rhs)
          List(state(q, 0)(close) -> dflt, state(q)(eos) -> dflt)
      } ++ List(
        state(q)(open) -> ERhs.Call(q, Depth.Increment, params.map(ERhs.Param(_))),
        state(q)(close) -> ERhs.Call(q, Depth.Decrement, params.map(ERhs.Param(_))),
        state(q)(__) -> ERhs.Call(q, Depth.Copy, params.map(ERhs.Param(_)))
      )
    }

    val compiler = new pattern.Compiler[F, ETag[In], Pattern[In], ERhs[Out]](Pattern.heuristic)

    compiler.compile(cases).map(new ESP(init, rules.view.mapValues(_.params).toMap, _))
  }

}
