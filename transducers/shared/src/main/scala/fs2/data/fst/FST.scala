/*
 * Copyright 2021 Lucas Satabin
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
package fst

import transducer._

import cats.syntax.all._
import cats.{Monoid, Show}

/** Non-deterministis finit state transducer.
  * To be well-formed, for any state, there is either
  *  - epsilon transitions, or
  *  - symbol transitions but
  *  - not both
  */
case class FST[Q, Pred, Fun, In, Out](initial: Q,
                                      states: Set[Q],
                                      edges: OrderedEdgeSet[Q, Pred, Fun, Out],
                                      finals: Set[Q])(implicit Fun: Func.Aux[Fun, In, Out]) {

  def isChoiceState(q: Q): Boolean =
    edges.forwardEpsilon.contains(q)

  def isSkipState(q: Q): Boolean =
    edges.forwardEpsilon.get(q) match {
      case Some(List(_)) => true
      case _             => false
    }

  def isJoinState(q: Q): Boolean =
    edges.backward.getOrElse(q, Nil).size + edges.backwardEpsilon.getOrElse(q, Nil).size > 1

  def enumerateStates: FST[Int, Pred, Fun, In, Out] = {
    val q2int = states.toList.zipWithIndex.toMap
    val intedges = OrderedEdgeSet.fromList[Int, Pred, Fun, Out](edges.toList.map { case (src, act, tgt) =>
      (q2int(src), act, q2int(tgt))
    })
    FST(q2int(initial), q2int.values.toSet, intedges, finals.map(q2int(_)))
  }

  def evalEdges(q: Q, in: In)(implicit Pred: SetLike[Pred, In]): List[(Out, Q)] =
    edges.forward.get(q) match {
      case None => Nil
      case Some(ts) =>
        ts.flatMap { case (pred, f, q) =>
          if (pred.contains(in))
            List((Fun.eval(f)(in), q))
          else
            Nil
        }
    }

  def rightClosure(q: Q)(implicit Out: Monoid[Out]): List[(Out, Q)] = {
    def go(q: Q, visited: Set[Q], out: Out): (Set[Q], List[(Out, Q)]) =
      edges.forwardEpsilon.get(q) match {
        case Some(Nil) | None =>
          (visited, List(out -> q))
        case Some(ts) =>
          ts.foldLeft((visited, List.empty[(Out, Q)])) { case ((visited, acc), (w, q1)) =>
            if (visited.contains(q1)) {
              (visited, acc)
            } else {
              val (visited1, ys) = go(q1, visited + q1, out.combine(w))
              (visited1, acc ++ ys)
            }
          }
      }
    go(q, Set.empty, Out.empty)._2
  }

  def pipe[F[_]: RaiseThrowable](
      emitEarly: Boolean = true)(implicit Out: Monoid[Out], Pred: SetLike[Pred, In]): Pipe[F, In, Out] =
    new FSTPipe(this, emitEarly)

}

object FST {

  implicit def show[Q: Show, pred: Show, F: Show, In, Out: Show]: Show[FST[Q, pred, F, In, Out]] = Show.show { fst =>
    val transitions = fst.states.toList.map { q =>
      if (fst.edges.forward.contains(q))
        fst.edges.forward(q).map { case (pred, f, tgt) => show"$q - $pred / $f -> $tgt" }
      else
        fst.edges.forwardEpsilon.getOrElse(q, Nil).map { case (out, tgt) => show"$q - / $out -> $tgt" }
    }
    show"""FST {
          |  initial = ${fst.initial}
          |  ${transitions.mkString_("\n  ")}
          |  finals = ${fst.finals}
          |}""".stripMargin
  }

}

case class OrderedEdgeSet[Q, Pred, F, Out](forward: Map[Q, List[(Pred, F, Q)]],
                                           backward: Map[Q, List[(Pred, F, Q)]],
                                           forwardEpsilon: Map[Q, List[(Out, Q)]],
                                           backwardEpsilon: Map[Q, List[(Out, Q)]])(implicit F: Func.Range[F, Out]) {

  def toList: List[Edge[Q, Pred, F, Out]] = {
    val sym = forward.toList.flatMap { case (q, ts) => ts.map { case (pred, fun, tgt) => (q, Left((pred, fun)), tgt) } }
    val eps = forwardEpsilon.toList.flatMap { case (q, ts) => ts.map { case (out, tgt) => (q, Right(out), tgt) } }
    sym ++ eps
  }

}

object OrderedEdgeSet {

  def fromList[Q, Pred, Fun, Out](edges: List[Edge[Q, Pred, Fun, Out]])(implicit
      Fun: Func.Range[Fun, Out]): OrderedEdgeSet[Q, Pred, Fun, Out] =
    OrderedEdgeSet(
      forward = edges.collect { case (q, Left((a, b)), q1) => Map(q -> List((a, b, q1))) }.combineAll,
      backward = edges.collect { case (q, Left((a, b)), q1) => Map(q1 -> List((a, b, q))) }.combineAll,
      forwardEpsilon = edges.collect { case (q, Right(out), q1) => Map((q -> List((out, q1)))) }.combineAll,
      backwardEpsilon = edges.collect { case (q, Right(out), q1) => Map((q1 -> List((out, q)))) }.combineAll
    )

}
