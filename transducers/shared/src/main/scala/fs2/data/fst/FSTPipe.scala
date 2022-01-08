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

import transducer.SetLike

import cats.Monoid

case class FSTException(msg: String) extends Exception(msg)

private class FSTPipe[F[_]: RaiseThrowable, In, Out: Monoid, Q, Pred, Fun](fst: FST[Q, Pred, Fun, In, Out],
                                                                           emitEarly: Boolean)(implicit
    Pred: SetLike[Pred, In])
    extends Pipe[F, In, Out] {
  def apply(s: Stream[F, In]): Stream[F, Out] = {
    def go(chunk: Chunk[In], idx: Int, rest: Stream[F, In], outs: List[(List[Out], Q)]): Pull[F, Out, Unit] =
      if (idx >= chunk.size) {
        rest.pull.uncons.flatMap {
          case Some((hd, tl)) => go(hd, 0, tl, outs)
          case None =>
            outs.filter(p => fst.finals.contains(p._2)) match {
              case (outs, q) :: _ =>
                Pull.output(Chunk.seq(outs.reverse))
              case _ =>
                Pull.raiseError(FSTException("invalid input"))
            }
        }
      } else {
        val in = chunk(idx)

        def prune(visited: Set[Q], l: List[(List[Out], Q)]): List[(List[Out], Q)] =
          l match {
            case Nil => Nil
            case (outs, q) :: rest =>
              if (visited.contains(q))
                prune(visited, rest)
              else
                (outs, q) :: prune(visited + q, rest)
          }

        def close(l: List[(List[Out], Q)]) =
          prune(Set.empty,
                l.flatMap { case (outs, q) => fst.rightClosure(q).map { case (out, q) => (out :: outs, q) } })

        def step(outs: List[(List[Out], Q)]) =
          outs.flatMap { case (outs, q) =>
            fst.evalEdges(q, in).map { case (out, q) => (out :: outs, q) }
          }

        val outs1 = close(step(outs))

        outs1 match {
          case List((outs, q)) if emitEarly && fst.finals.contains(q) =>
            Pull.output(Chunk.seq(outs.reverse)) >> go(chunk, idx + 1, rest, List((Nil, q)))
          case _ =>
            go(chunk, idx + 1, rest, outs1)
        }
      }

    go(Chunk.empty, 0, s, fst.rightClosure(fst.initial).map { case (out, q) => (List(out), q) }).stream
  }

}
