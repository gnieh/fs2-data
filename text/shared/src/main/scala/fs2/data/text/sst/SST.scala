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
package text
package sst

import scala.annotation.tailrec

class SST(val states: Set[State],
          val transitions: Map[State, Transitions],
          val init: State,
          val finals: Map[State, List[Value]]) {

  private val firstChars = epsilonReachableChars(List(init), Set.empty, CharRanges.empty)

  @tailrec
  final def epsilonReachableChars(from: List[State], visited: Set[State], acc: CharRanges): CharRanges = from match {
    case Nil => acc
    case q :: rest =>
      transitions.get(q) match {
        case Some(Transitions.Symbol(rs, _)) =>
          epsilonReachableChars(rest, visited + q, acc.merge(rs))
        case Some(Transitions.Skip(Transition(_, tgt))) if !visited.contains(tgt) =>
          epsilonReachableChars(tgt :: rest, visited + q, acc)
        case Some(Transitions.Choice(Transition(_, tgt1), Transition(_, tgt2))) =>
          val newFrom = if (visited.contains(tgt2)) from else tgt2 :: from
          val newFrom1 = if (visited.contains(tgt1)) newFrom else tgt1 :: newFrom
          epsilonReachableChars(newFrom1, visited + tgt1 + tgt2, acc)
        case _ =>
          epsilonReachableChars(rest, visited + q, acc)
      }
  }

  /** Determines whether this character can start the SST. */
  def isStart(c: Char): Boolean =
    firstChars.contains(c)

  /** A resting state is either final or a state with a single symbol transition. */
  def isResting(q: State): Boolean =
    finals.contains(q) || transitions.get(q).exists {
      case Transitions.Symbol(_, _) => true
      case _                        => false
    }

  def isFinal(q: State): Boolean =
    finals.contains(q)

  def step(state: State, c: Char): Option[Transition] =
    transitions.get(state).collect {
      case Transitions.Symbol(rs, t) if rs.contains(c) => t
    }

}

sealed trait Transitions
object Transitions {
  case class Choice(first: Transition, second: Transition) extends Transitions
  case class Skip(t: Transition) extends Transitions
  case class Symbol(ranges: CharRanges, t: Transition) extends Transitions
}

case class Transition(updates: List[RegisterUpdate], target: State)

case class RegisterUpdate(idx: Int, values: List[Value])

sealed trait UpdateValue
object UpdateValue {
  case object CurrentChar extends UpdateValue
  case class Val(v: Value) extends UpdateValue
}
