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
package automaton

import cats.effect.Concurrent
import cats.effect.std.Queue
import cats.syntax.all._
import cats.data.NonEmptyList

/** A pipe that allows abstract implementation of recursive queries on tree like structures.
  *
  * The pipe assumes a structure that has open and end tokens to describe the tree structure.
  *
  * A context might be provided to create the element to match against an open.
  *
  * It is appropriated to implement query languages such as XPath or JsonPath.
  */
abstract class TreeQueryPipe[F[_]: Concurrent, T, O <: T, Matcher, Matchable](dfa: PDFA[Matcher, Matchable])
    extends Pipe[F, T, Stream[F, T]] {

  /** Whether to emit open and close tags on new match. */
  val emitOpenAndClose: Boolean = true

  /** Creates the element to match given an opening token and the current context. */
  def makeMatchingElement(open: O): Matchable

  /** Specializes the token type to the opening ones. */
  def isOpen(tok: T): Option[O]

  /** Specializes the token type to the closing ones. */
  def isClose(tok: T): Boolean

  private object Open {
    def unapply(tok: T) = isOpen(tok)
  }

  private object Close {
    def unapply(tok: T) = isClose(tok)
  }

  private def go(chunk: Chunk[T],
                 idx: Int,
                 rest: Stream[F, T],
                 depth: Int,
                 queues: List[(Int, Queue[F, Option[T]])],
                 resetting: Boolean,
                 q: NonEmptyList[(Int, Boolean)]): Pull[F, Stream[F, T], Unit] =
    if (idx >= chunk.size) {
      rest.pull.uncons.flatMap {
        case Some((hd, tl)) => go(hd, 0, tl, depth, queues, resetting, q)
        case None           => Pull.done
      }
    } else {
      chunk(idx) match {
        case tok @ Close() =>
          val (top, nested) = queues.span(_._1 == depth - 1)
          Pull.eval((if (emitOpenAndClose) queues else nested).traverse_(_._2.offer(tok.some))) >> Pull.eval(
            top.traverse_(_._2.offer(none))) >> go(
            chunk,
            idx + 1,
            rest,
            depth - 1,
            nested,
            q.tail.headOption.fold(false)(_._2),
            NonEmptyList.fromList(q.tail).getOrElse(NonEmptyList.one((dfa.init, false))))
        case Open(tok) =>
          dfa.step(q.head._1, makeMatchingElement(tok)) match {
            case Some(q1) =>
              val updateQueues =
                if (!resetting && dfa.finals.contains(q1)) {
                  // this is a new match, spawn a new down stream
                  Pull.eval(Queue.unbounded[F, Option[T]]).flatMap { queue =>
                    Pull.output1(Stream.fromQueueNoneTerminated(queue, 1)).as((depth, queue) :: queues)
                  }
                } else {
                  Pull.pure(queues)
                }
              updateQueues
                .evalMap(queues =>
                  (if (emitOpenAndClose) queues else queues.dropWhile(_._1 == depth))
                    .traverse_(_._2.offer(tok.some))
                    .as(queues))
                .flatMap(go(chunk, idx + 1, rest, depth + 1, _, resetting, (q1, resetting) :: q))
            case None =>
              Pull.eval(queues.traverse_(_._2.offer(tok.some))) >> go(chunk,
                                                                      idx + 1,
                                                                      rest,
                                                                      depth + 1,
                                                                      queues,
                                                                      true,
                                                                      (q.head._1, resetting) :: q)
          }
        case tok =>
          Pull
            .eval(queues.traverse_(_._2.offer(tok.some))) >> go(chunk, idx + 1, rest, depth, queues, resetting, q)
      }
    }

  final def apply(s: Stream[F, T]): Stream[F, Stream[F, T]] =
    go(Chunk.empty, 0, s, 0, Nil, false, NonEmptyList.one((dfa.init, false))).stream

}
