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

package fs2.data
package mft
package query

import pfsa.{Candidate, Pred, Regular}
import cats.Eq
import cats.syntax.all._
import cats.data.NonEmptyList
import scala.annotation.tailrec

/** This compiler can be used to compile to an MFT any query language that can be represented by nested for loops.
  *
  * The compiler is based on the approach described in [[https://doi.org/10.1109/ICDE.2014.6816714 _XQuery Streaming by Forest Transducers_]]
  * and generalized for the abstract query language on trees.
  */
abstract class QueryCompiler[Tag, Path] {

  type Matcher
  type Pattern
  type Guard

  /** A single char to be matched in a path */
  type Char

  implicit def predicate: Pred[Matcher, Char]

  implicit def candidate: Candidate[Matcher, Char]

  implicit def charsEq: Eq[Matcher]

  /** Creates a regular expression given a path. */
  def path2regular(path: Path): Regular[Matcher]

  /** Create (ordered) pattern matching cases with guards for a given matcher.
    *
    * Guard is expressed as a conjunction of atomic guard operations.
    * If a case has no guard, returns an empty list for this case.
    *
    * Cases will be matched in ordered, the first matching case will be taken.
    */
  def cases(matcher: Matcher): List[(Pattern, List[Guard])]

  /** Return the constructor tag of this pattern, or `None` if it is a wildcard. */
  def tagOf(pattern: Pattern): Option[Tag]

  def compile(query: Query[Tag, Path]): MFT[NonEmptyList[Guard], Tag, Tag] = dsl { implicit builder =>
    val q0 = state(args = 0, initial = true)
    val qinit = state(args = 1)
    val qcopy = state(args = 0)

    qcopy(anyNode) -> copy(qcopy(x1)) ~ qcopy(x2)
    qcopy(anyLeaf) -> copy
    qcopy(epsilon) -> eps

    // input is copied in the first argument
    q0(any) -> qinit(x0, qcopy(x0))

    def translatePath(path: Path, start: builder.StateBuilder, end: builder.StateBuilder): Unit = {
      val dfa = path2regular(path).deriveDFA
      // resolve transitions into patterns and guards
      val resolvedTransitions =
        dfa.transitions.toList.zipWithIndex.flatMap { case (transitions, src) =>
          transitions.flatMap { case (cond, tgt) =>
            cases(cond).map { case (pat, guard) =>
              (src, pat, NonEmptyList.fromList(guard), tgt)
            }
          }
        }
      val outgoingTransitions = resolvedTransitions.groupMapReduce(_._1)(t => Set(t._4))(_ ++ _)
      val incomingTransitions = resolvedTransitions.groupMapReduce(_._4)(t => Set(t._1))(_ ++ _)
      @tailrec
      def dropUnreachableStates(qs: List[Int], visited: Set[Int], reach: Map[Int, Set[Int]]): Set[Int] =
        qs match {
          case Nil => visited
          case q :: qs =>
            if (visited.contains(q))
              dropUnreachableStates(qs, visited, reach)
            else
              dropUnreachableStates(qs ++ reach.getOrElse(q, Set.empty).diff(visited), visited + q, reach)
        }
      // keep only the states that are part of realisable paths
      val reachableStates = dropUnreachableStates(List(dfa.init), Set.empty, outgoingTransitions)
      val aliveStates = dropUnreachableStates(dfa.finals.toList, Set.empty, incomingTransitions)
      // we now have the final transitions
      val finalTransitions = resolvedTransitions
        .filter { case (src, _, _, tgt) =>
          reachableStates.contains(src) && aliveStates.contains(tgt)
        }
        .groupMap(_._1)(t => (t._2, t._3, t._4))
      // we can apply the DFA to MFT translation now
      finalTransitions.foldLeft(Map(dfa.init -> start)) { case (states, (src, transitions)) =>
        val initialSrc = src === dfa.init
        val (q1, states1) =
          states.get(src) match {
            case Some(q1) => (q1, states)
            case None =>
              val q1 =
                if (initialSrc)
                  start
                else
                  state(args = start.nargs)
              (q1, states.updated(src, q1))
          }
        val copyArgs = List.tabulate(q1.nargs)(y(_))
        transitions.foldLeft(states1) { case (states, (pattern, guard, tgt)) =>
          val finalTgt = dfa.finals.contains(tgt)
          val (q2, states1) =
            states.get(tgt) match {
              case Some(q2) => (q2, states)
              case None =>
                val q2 = state(args = q1.nargs)
                (q2, states.updated(tgt, q2))
            }
          val pat: builder.Guardable = tagOf(pattern).fold(anyNode)(aNode(_))
          if (!initialSrc && !finalTgt) {
            q1(pat.when(guard)) -> q2(x1, copyArgs: _*) ~ q1(x2, copyArgs: _*)
          } else if (initialSrc && !finalTgt) {
            q1(pat.when(guard)) -> q2(x1, copyArgs: _*)
          } else if (!initialSrc && finalTgt) {
            q1(pat.when(guard)) -> end(x0, (copyArgs :+ copy(qcopy(x1))): _*) ~ q2(x1, copyArgs: _*)
          } else {
            q1(pat.when(guard)) -> end(x1, (copyArgs :+ copy(qcopy(x1))): _*) ~ q2(x1, copyArgs: _*)
          }
          states1
        }
      }: Unit
    }

    def translate(query: Query[Tag, Path], vars: List[String], q: builder.StateBuilder): Unit =
      query match {
        case Query.ForClause(variable, source, result) =>
          val q1 = state(args = q.nargs + 1)

          // compile the variable binding path
          translatePath(source, q, q1)

          // then the body with the bound variable
          translate(result, variable :: vars, q1)

        case Query.LetClause(variable, query, result) =>
          val qv = state(args = q.nargs)
          val q1 = state(args = q.nargs + 1)

          // compile the variable binding query
          translate(query, vars, qv)
          // then the body with the bound variable
          translate(result, variable :: vars, q1)

          // bind everything
          val copyArgs = List.tabulate(q.nargs)(y(_))
          q(any) -> q1(x0, (copyArgs :+ qv(x0, copyArgs: _*)): _*)

        case Query.Ordpath(path) =>
          val q1 = state(args = q.nargs + 1)

          // compile the path
          translatePath(path, q, q1)

          // emit the result
          q1(any) -> y(q.nargs)
        case Query.Element(tag, Some(child)) =>
          val q1 = state(args = q.nargs)

          // translate the child query
          translate(child, vars, q1)

          // bind it
          val copyArgs = List.tabulate(q.nargs)(y(_))
          q(any) -> node(tag)(q1(x0, copyArgs: _*))

        case Query.Element(tag, None) =>
          // just emit it
          q(any) -> leaf(tag)

        case Query.Variable(name) =>
          // variable named are pushed on top of the list, so indexing is reversed
          q(any) -> y(vars.size - 1 - vars.indexOf(name))

        case Query.Sequence(queries) =>
          val copyArgs = List.tabulate(q.nargs)(y(_))

          // compile and sequence every query in the sequence
          val rhs =
            queries.foldLeft[Rhs[Tag]](eps) { (acc, query) =>
              val q1 = state(args = q.nargs)

              // translate the query
              translate(query, vars, q1)

              acc ~ q1(x0, copyArgs: _*)
            }

          // emit rhs for any input
          q(any) -> rhs
      }

    translate(query, List("$input"), qinit)
  }

}
