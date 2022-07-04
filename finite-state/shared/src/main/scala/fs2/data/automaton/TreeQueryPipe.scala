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

/** A pipe that allows abstract implementation of recursive queries on tree like structures.
  *
  * The pipe assumes a structure that has open and end tokens to describe the tree structure.
  *
  * A context might be provided to create the element to match against an open.
  *
  * It is appropriated to implement query languages such as XPath or JsonPath.
  */
abstract class TreeQueryPipe[F[_]: Concurrent, T, O <: T, C <: T, Matcher, Matchable, Ctx](
    dfa: PDFA[Matcher, Matchable])
    extends Pipe[F, T, Stream[F, T]] {

  /** Initializes a new context upon start. */
  def initCtx: Ctx

  /** Pushes a new depth in the context when encountering an open tag. */
  def push(open: O, ctx: Ctx): Ctx

  /** Pops one depth from the context when encountering a close tag. */
  def pop(close: C, ctx: Ctx): Ctx

  /** Updates the context upon starting a new element.
    * An element is either:
    *  - a leaf element
    *  - a well balanced open/.../close sequence
    */
  def update(ctx: Ctx): Ctx

  /** Creates the element to match given an opening token and the current context. */
  def makeMatchingElement(open: O, ctx: Ctx): Matchable

  /** Creates an element to match given the current context, without consuming any token. */
  def noTokenMatchable(ctx: Ctx): Option[Matchable]

  /** The current depth in the tree structure based on the context. */
  def depth(ctx: Ctx): Int

  /** Specializes the token type to the opening ones. */
  def isOpen(tok: T): Option[O]

  /** Specializes the token type to the closing ones. */
  def isClose(tok: T): Option[C]

  private object Open {
    def unapply(tok: T) = isOpen(tok)
  }

  private object Close {
    def unapply(tok: T) = isClose(tok)
  }

  private def go(chunk: Chunk[T],
                 idx: Int,
                 rest: Stream[F, T],
                 ctx: Ctx,
                 queues: List[(Int, Queue[F, Option[T]])],
                 resetting: Boolean,
                 q: Int): Pull[F, Stream[F, T], Unit] =
    if (idx >= chunk.size) {
      rest.pull.uncons.flatMap {
        case Some((hd, tl)) => go(hd, 0, tl, ctx, queues, resetting, q)
        case None           => Pull.done
      }
    } else {
      chunk(idx) match {
        case Close(tok) =>
          val d = depth(ctx)
          val (top, nested) = queues.span(_._1 == d - 1)
          Pull.eval(queues.traverse_(_._2.offer(tok.some))) >> Pull
            .eval(top.traverse_(_._2.offer(none))) >> go(chunk,
                                                         idx + 1,
                                                         rest,
                                                         pop(tok, ctx),
                                                         nested,
                                                         if (d == 1) false else resetting,
                                                         if (d == 1) dfa.init else q)
        case tok =>
          // on non closing tokens, we might have a DFA transition to take before reading the next token
          // the next token begins a new element (either a leaf or a sub-tree), some processors, might
          // need this hook
          tok match {
            case Open(tok) =>
              noTokenMatchable(ctx)
                .fold(q.some)(dfa.step(q, _))
                .flatMap(dfa.step(_, makeMatchingElement(tok, ctx))) match {
                case Some(q) =>
                  val updateQueues =
                    if (!resetting && dfa.finals.contains(q)) {
                      // this is a new match, spawn a new down stream
                      Pull.eval(Queue.unbounded[F, Option[T]]).flatMap { queue =>
                        Pull.output1(Stream.fromQueueNoneTerminated(queue, 1)).as((depth(ctx), queue) :: queues)
                      }
                    } else {
                      Pull.pure(queues)
                    }
                  updateQueues
                    .evalMap { queues =>
                      queues.traverse_(_._2.offer(tok.some)).as(queues)
                    }
                    .flatMap(go(chunk, idx + 1, rest, push(tok, update(ctx)), _, resetting, q))
                case None =>
                  Pull.eval(queues.traverse_(_._2.offer(tok.some))) >> go(chunk,
                                                                          idx + 1,
                                                                          rest,
                                                                          push(tok, update(ctx)),
                                                                          queues,
                                                                          true,
                                                                          q = q)
              }
            case _ =>
              val q1 =
                noTokenMatchable(ctx).fold(q)(dfa.step(q, _).getOrElse(q))
              Pull
                .eval(queues.traverse_(_._2.offer(tok.some))) >> go(chunk,
                                                                    idx + 1,
                                                                    rest,
                                                                    update(ctx),
                                                                    queues,
                                                                    resetting,
                                                                    q1)
          }
      }
    }

  final def apply(s: Stream[F, T]): Stream[F, Stream[F, T]] =
    go(Chunk.empty, 0, s, initCtx, Nil, false, dfa.init).stream

}
