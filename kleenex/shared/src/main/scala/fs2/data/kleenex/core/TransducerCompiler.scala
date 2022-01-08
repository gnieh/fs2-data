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

package fs2.data
package kleenex
package core

import fst._
import transducer.CharRanges

import cats.data.NonEmptyList
import cats.MonadError
import cats.syntax.all._

class TransducerCompiler[F[_]](implicit F: MonadError[F, Throwable]) {

  private type Q = List[Int]
  private type E =
    Edge[List[Int], CharRanges, CopyFunc[Char, List[Either[String, Action]]], List[Either[String, Action]]]

  def build(prog: Program): F[NonEmptyList[Transducer[Int, Char, Either[String, Action]]]] =
    prog.pipeline.traverse { ident =>
      construct(prog, ident).map(_.enumerateStates)
    }

  private def construct(prog: Program, ident: Int): F[Transducer[Q, Char, Either[String, Action]]] = {
    def decl(id: Int): F[Term] =
      prog.decls.get(id) match {
        case Some(t) => t.pure[F]
        case None    => F.raiseError(KleenexCompilerException(s"Unknown identifier $id"))
      }
    // Optimization: Reduce number of generated states by contracting
    // non-deterministic edges with no output. This is done by "skipping" states
    // whose head nonterminal is declared to be a Seq term, or an RSum with only
    // one successor.
    def follow(qs: Q): F[Q] =
      qs match {
        case Nil => List.empty.pure[F]
        case q :: qs1 =>
          decl(q).flatMap {
            case Term.Seq(rs)                           => follow(rs ++ qs1)
            case Term.Alternative(NonEmptyList(r, Nil)) => follow(r :: qs1)
            case _                                      => qs.pure[F]
          }
      }

    def go(workingSet: List[Q], states: Set[Q], transitions: List[E]): F[(Set[Q], List[E])] =
      workingSet match {
        case Nil =>
          (states, transitions).pure[F]
        case q :: rest if states.contains(q) =>
          go(rest, states, transitions)
        case Nil :: rest =>
          go(rest, states + Nil, transitions)
        case (h @ q :: qs) :: rest =>
          val states1 = states + h
          decl(q).flatMap {
            case Term.Const(out) =>
              follow(qs).flatMap { q1 =>
                go(q1 :: rest, states1, (h, Right(List(out)), q1) :: transitions)
              }
            case Term.Read(pred, false) =>
              follow(qs).flatMap { q1 =>
                go(q1 :: rest, states1, (h, Left((pred, CopyFunc.CopyConst(Nil))), q1) :: transitions)
              }
            case Term.Read(pred, true) =>
              follow(qs).flatMap { q1 =>
                go(q1 :: rest, states1, (h, Left((pred, CopyFunc.CopyArg)), q1) :: transitions)
              }
            case Term.Seq(rs) =>
              follow(rs ++ qs).flatMap { q1 =>
                go(q1 :: rest, states1, (h, Right(Nil), q1) :: transitions)
              }
            case Term.Alternative(rs) =>
              rs.toList.traverse(r => follow(r :: qs)).flatMap { qs1 =>
                val trans = qs1.map(q1 => (h, Right(Nil), q1))
                go(qs1 reverse_::: rest, states1, trans ++ transitions)
              }
          }
      }

    go(List(List(ident)), Set.empty, Nil).map { case (states, transitions) =>
      new FST(List(ident), states, OrderedEdgeSet.fromList(transitions), Set(Nil))
    }
  }

}
