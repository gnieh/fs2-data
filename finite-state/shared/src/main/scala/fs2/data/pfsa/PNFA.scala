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
package pfsa

import Pred.syntax._

import cats.syntax.all._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuilder

private[data] class PNFA[P, T](val init: Int, val finals: Set[Int], val transitions: Map[Int, List[(Option[P], Int)]])(
    implicit P: Pred[P, T]) {

  private def epsilonClosure(qs: List[Int]): Set[Int] = {
    @tailrec
    def loop(qs: List[Int], visited: Set[Int]): Set[Int] =
      qs match {
        case q :: qs =>
          if (visited.contains(q))
            loop(qs, visited)
          else
            loop(transitions.getOrElse(q, Nil).collect { case (None, q1) => q1 } reverse_::: qs, visited + q)
        case Nil => visited
      }
    loop(qs, Set.empty)
  }

  /** Generates the stream of all combinations of predicates splitting in two sets:
    *  - predicates to match
    *  - predicates to negate
    * and then combining them with conjunction.
    * Returns only predicate that are not obviously non satisfiable.
    */
  private def combineAll(predicates: List[(P, Set[Int])]): Stream[Pure, (P, Set[Int])] =
    Stream
      .range(1, predicates.size + 1)
      .flatMap(n =>
        Stream.unfold(predicates.combinations(n))(combinations =>
          if (combinations.hasNext) Some((combinations.next(), combinations)) else None))
      .map { pos =>
        val neg = predicates.diff(pos)
        (neg.foldLeft(pos.foldLeft(P.always)((p1, p2) => p1 && p2._1))((p1, p2) => p1 && !p2._1),
         pos.foldLeft(Set.empty[Int])(_ union _._2))
      }
      .filter(_._1.isSatisfiable)

  /** Returns the deterministic finite state automaton with predicate having the same meaning as this one.
    * Determinization is based on the approach described in paper _Finite State Transducers with Predicates and Identities_
    * by Gertjan van Noord and Dale Gerdemann.
    */
  def determinize: PDFA[P, T] = {

    @tailrec
    def loop(toProcess: List[(Set[Int])],
             newStates: Map[Set[Int], Int],
             newFinals: Set[Set[Int]],
             newTransitions: ArrayBuilder[List[(P, Set[Int])]]): PDFA[P, T] =
      toProcess match {
        case Nil =>
          new PDFA[P, T](0,
                         newFinals.map(newStates(_)),
                         None,
                         newTransitions.result().map(_.map { case (p, q) => (p, newStates(q)) }))
        case q :: qs =>
          if (newStates.contains(q)) {
            loop(qs, newStates, newFinals, newTransitions)
          } else {
            val newQ = newStates.size
            val newStates1 = newStates.updated(q, newQ)
            val ts = q.toList
              .flatMap(transitions.get(_))
              .flatMap(_.collect { case (Some(p), q) =>
                (p, q)
              })
              .groupBy(_._1)
              .fmap(_.map(_._2))
              .fmap(epsilonClosure(_))
              .toList
            val ts1 =
              combineAll(ts).compile.toList
            val newTransistions1 = if (ts1.nonEmpty) newTransitions += ts1 else newTransitions
            val newFinals1 = if (finals.exists(q.contains(_))) newFinals + q else newFinals
            loop(qs ++ ts1.map(_._2), newStates1, newFinals1, newTransistions1)
          }
      }

    loop(List(epsilonClosure(List(init))), Map.empty, Set.empty, Array.newBuilder)
  }

}
