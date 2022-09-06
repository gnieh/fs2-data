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

import esp.{Depth, ESP, Rhs => ERhs, Pattern, PatternDsl, Tag => ETag}

import cats.{Defer, MonadError}
import cats.syntax.all._

import scala.collection.compat._

sealed trait Forest
object Forest {
  case object Self extends Forest
  case object First extends Forest
  case object Second extends Forest
}

sealed trait EventSelector[+InTag]
object EventSelector {
  case object AnyNode extends EventSelector[Nothing]
  case class Node[InTag](tag: InTag) extends EventSelector[InTag]
  case object AnyLeaf extends EventSelector[Nothing]
  case class Leaf[InTag](v: InTag) extends EventSelector[InTag]
  case object Epsilon extends EventSelector[Nothing]
}

sealed trait Rhs[+OutTag] {
  def ~[OutTag1 >: OutTag](that: Rhs[OutTag1]): Rhs[OutTag1] =
    Rhs.Concat(this, that)
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
}

/** A Macro Forest Transducer, as described in _Streamlining Functional XML Processing_.
  * To each state is associated a collection of rules, matching a forest and
  * generating a new one.
  *
  * An MFT is an intermediate structure towards a compiled [[fs2.data.esp.ESP Events Stream Processor]]
  */
private[data] class MFT[InTag, OutTag](init: Int, rules: Map[Int, Rules[InTag, OutTag]]) {

  /** Compiles this MFT into an ESP.
    * The generated ESP contains one decision tree encoding all the patterns
    * of this MFT.
    */
  def esp[F[_]](implicit F: MonadError[F, Throwable], defer: Defer[F]): F[ESP[F, InTag, OutTag]] = {

    val dsl = new PatternDsl[InTag]
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
        case (EventSelector.Node(tag), rhs) =>
          List(state(q, 0)(open(tag, none)) -> translateRhs(rhs))
        case (EventSelector.AnyNode, rhs) =>
          List(state(q, 0)(open(as = "in")) -> translateRhs(rhs))
        case (EventSelector.Leaf(in), rhs) =>
          List(state(q, 0)(value(in)) -> translateRhs(rhs))
        case (EventSelector.AnyLeaf, rhs) =>
          List(state(q, 0)(value(as = "in")) -> translateRhs(rhs))
        case (EventSelector.Epsilon, rhs) =>
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
      new pattern.Compiler[F, ETag[InTag], Pattern[InTag], ERhs[OutTag]](Pattern.heuristic)

    compiler.compile(cases).map(new ESP(init, rules.view.mapValues(_.params).toMap, _))
  }

}
