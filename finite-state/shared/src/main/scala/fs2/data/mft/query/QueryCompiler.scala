/*
 * Copyright 2023 Lucas Satabin
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

/** This compiler can be used to compile to an MFT any query language that can be represented by nested for loops.
  *
  * The compiler is based on the approach described in [[https://doi.org/10.1109/ICDE.2014.6816714 _XQuery Streaming by Forest Transducers_]]
  * and generalized for the abstract query language on trees.
  */
private[fs2] abstract class QueryCompiler[InTag, OutTag, Path] {

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
  def tagOf(pattern: Pattern): Option[InTag]

  /** Compiles the `query` into an [[MFT Macro Forest Transducer]].
    * The `credit` parameter defines the maximum number of optimization passes that
    * are performed on the resulting MFT.
    * Optimization passes allow to reduce the size of the generated MFT
    * and used parameters, which improve the runtime characteristics of the
    * results.
    *
    * If you do not want to perform any optimization, you can set this value to `0`.
    */
  def compile(query: Query[OutTag, Path], credit: Int = 50): MFT[NonEmptyList[Guard], InTag, OutTag] = {
    val mft = dsl[NonEmptyList[Guard], InTag, OutTag] { implicit builder =>
      val q0 = state(args = 0, initial = true)
      val qinit = state(args = 1)
      val qcopy = state(args = 0)

      qcopy(anyNode) -> copy(qcopy(x1)) ~ qcopy(x2)
      qcopy(anyLeaf) -> copy ~ qcopy(x1)
      qcopy(epsilon) -> eps

      // input is copied in the first argument
      q0(any) -> qinit(x0, qcopy(x0))

      def translatePath(path: Path, start: builder.StateBuilder, end: builder.StateBuilder): Unit = {
        val regular = path2regular(path)
        val dfa = regular.deriveDFA
        // resolve transitions into patterns and guards
        val transitionCases =
          dfa.transitions.toList.zipWithIndex.map { case (transitions, src) =>
            (src,
             transitions.flatMap { case (cond, tgt) =>
               cases(cond).map { case (pat, guard) =>
                 (pat, NonEmptyList.fromList(guard), tgt)
               }
             })
          }.toMap
        // we can apply the DFA to MFT translation now
        val _ =
          transitionCases.foldLeft(Map(dfa.init -> start)) { case (states, (src, transitions)) =>
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
            val states2 =
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
                if (!finalTgt) {
                  q1(pat.when(guard)) -> q2(x1, copyArgs: _*) ~ q1(x2, copyArgs: _*)
                } else {
                  q1(pat.when(guard)) -> end(x1, (copyArgs :+ copy(qcopy(x1))): _*) ~ q2(x1, copyArgs: _*) ~
                    q1(x2, copyArgs: _*)
                }
                states1
              }
            q1(anyLeaf) -> eps
            q1(epsilon) -> eps
            states2
          }
      }

      def translate(query: Query[OutTag, Path], vars: List[String], q: builder.StateBuilder): Unit =
        query match {
          case Query.Empty() =>
            q(any) -> eps

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
          case Query.Node(tag, child) =>
            val q1 = state(args = q.nargs)

            // translate the child query
            translate(child, vars, q1)

            // bind it
            val copyArgs = List.tabulate(q.nargs)(y(_))
            q(any) -> node(tag)(q1(x0, copyArgs: _*))

          case Query.Leaf(tag) =>
            // just emit it
            q(any) -> leaf(tag)

          case Query.Variable(name) =>
            // variable named are pushed on top of the list, so indexing is reversed
            q(any) -> y(vars.size - 1 - vars.indexOf(name))

          case Query.Sequence(queries) =>
            val copyArgs = List.tabulate(q.nargs)(y(_))

            // compile and sequence every query in the sequence
            val rhs =
              queries.foldLeft[Rhs[OutTag]](eps) { (acc, query) =>
                val q1 = state(args = q.nargs)

                // translate the query
                translate(query, vars, q1)

                acc ~ q1(x0, copyArgs: _*)
              }

            // emit rhs for any input
            q(any) -> rhs

          case Query.LeafFunction(f) =>
            q(anyLeaf) -> applyToLeaf(f)
        }

      translate(query, List("$input"), qinit)
    }
    // apply some optimizations until nothing changes or credit is exhausted
    def optimize(mft: MFT[NonEmptyList[Guard], InTag, OutTag], credit: Int): MFT[NonEmptyList[Guard], InTag, OutTag] =
      if (credit > 0) {
        val mft1 = mft.removeUnusedParameters.inlineStayMoves.removeUnreachableStates
        if (mft1.rules == mft.rules)
          mft
        else
          optimize(mft1, credit - 1)
      } else {
        mft
      }
    optimize(mft, credit)
  }

}
