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
package stt

import transducer._

import cats.syntax.all._
import cats.MonadError
import cats.data.OptionT
import cats.data.Chain
import cats.Show

case class InternalTransition[Out](target: Int, update: List[Assignment[Variable.Normal, Out]])

case class CallTransition[Out, StackElem](target: Int, push: StackElem, update: List[Assignment[Variable.Normal, Out]])

case class ReturnTransition[Out](target: Int, update: List[Assignment[Variable, Out]])

/** A copyless streaming tree transducer implementation. */
class STT[F[_], T[_, _], In, Out, StackElem](initial: Int,
                                             internalTransitions: T[(Int, In), InternalTransition[Out]],
                                             callTransitions: T[(Int, In), CallTransition[Out, StackElem]],
                                             returnTransitions: T[(Int, StackElem, In), ReturnTransition[Out]],
                                             finalStates: Map[Int, Expr0[Out]],
                                             variables: Map[String, Type])(implicit
    F: MonadError[F, Throwable],
    T: Table[T],
    In: HasTag[In],
    showIn: Show[In],
    showOut: Show[Out])
    extends Pipe[F, In, Out] {

  def isFinal(state: Int): Boolean = finalStates.contains(state)

  def update[V <: Variable](env: Env[Variable, Out],
                            assignments: List[Assignment[V, Out]]): F[Env[Variable.Normal, Out]] = {
    def loop(assignments: List[Assignment[V, Out]], env: Env[Variable, Out]): F[Env[Variable.Normal, Out]] =
      assignments match {
        case assignment :: rest =>
          assignment match {
            case Assignment.Empty(x) =>
              loop(rest, env.update(x, Expr0.Empty()))
            case Assignment.Hole(x) =>
              loop(rest, env.update(x, Expr1.Hole()))
            case Assignment.Char(x, c) =>
              loop(rest, env.update(x, Expr0.Char(c)))
            case Assignment.Subtree(x, open, close) =>
              loop(rest, env.update(x, Expr1.Subtree(open, Expr1.Hole(), close)))
            case Assignment.Append(x, y) =>
              (env.lookupExpr[F](x), env.lookupExpr[F](y)).tupled.flatMap {
                case (Expr0(xe), Expr0(ye)) =>
                  loop(rest, env.update(x, Expr0.Concat(xe, ye)).update(y, Expr0.Empty()))
                case (Expr1(xe), Expr0(ye)) =>
                  loop(rest, env.update(x, Expr1.Concat10(xe, ye)).update(y, Expr0.Empty()))
                case (Expr0(xe), Expr1(ye)) =>
                  loop(rest, env.update(x, Expr1.Concat01(xe, ye)).update(y, Expr1.Hole()))
                case (Expr1(xe), Expr1(ye)) =>
                  F.raiseError(STTException("cannot append an expression of type 1 to another expression of type 1"))
              }
            case Assignment.Prepend(x, y) =>
              (env.lookupExpr[F](x), env.lookupExpr[F](y)).tupled.flatMap {
                case (Expr0(xe), Expr0(ye)) =>
                  loop(rest, env.update(x, Expr0.Concat(ye, xe)).update(y, Expr0.Empty()))
                case (Expr1(xe), Expr0(ye)) =>
                  loop(rest, env.update(x, Expr1.Concat01(ye, xe)).update(y, Expr0.Empty()))
                case (Expr0(xe), Expr1(ye)) =>
                  loop(rest, env.update(x, Expr1.Concat10(ye, xe)).update(y, Expr1.Hole()))
                case (Expr1(xe), Expr1(ye)) =>
                  F.raiseError(STTException("cannot prepend an expression of type 1 to another expression of type 1"))
              }
            case Assignment.SubstInX(x, y) =>
              (env.lookupExpr[F](x), env.lookupExpr[F](y)).tupled.flatMap {
                case (Expr1(xe), Expr1(ye)) =>
                  loop(rest, env.update(x, Expr1.Subst(xe, ye)).update(y, Expr1.Hole()))
                case (Expr1(xe), Expr0(ye)) =>
                  loop(rest, env.update(x, Expr0.Subst(xe, ye)).update(y, Expr0.Empty()))
                case (Expr0(xe), ye) =>
                  F.raiseError(STTException(show"cannot substitute in an expression of type 0: $xe"))
              }
            case Assignment.SubstInY(x, y) =>
              (env.lookupExpr[F](x), env.lookupExpr[F](y)).tupled.flatMap {
                case (Expr1(xe), Expr1(ye)) =>
                  loop(rest, env.update(x, Expr1.Subst(ye, xe)).update(y, Expr1.Hole()))
                case (Expr0(xe), Expr1(ye)) =>
                  loop(rest, env.update(x, Expr0.Subst(ye, xe)).update(y, Expr0.Empty()))
                case (xe, Expr0(ye)) =>
                  F.raiseError(STTException(show"cannot substitute in an expression of type 0: $ye"))
              }
            case Assignment.Swap(x, y) =>
              (env.lookupExpr[F](x), env.lookupExpr[F](y)).tupled.flatMap { case (xe, ye) =>
                loop(rest, env.update(x, ye).update(y, xe))
              }
          }
        case Nil => F.pure(env.destackify)
      }
    loop(assignments, env)
  }

  private def step(q: Int, stack: List[(StackElem, Env[Variable.Stack, Out])], env: Env[Variable.Normal, Out])(
      in: In): OptionT[F, (Int, List[(StackElem, Env[Variable.Stack, Out])], Env[Variable.Normal, Out])] =
    in match {
      case Tag.Internal() =>
        OptionT.fromOption(internalTransitions.get(q -> in)).semiflatMap { case InternalTransition(q1, upd) =>
          update(env.widen, upd).map((q1, stack, _))
        }
      case Tag.Call() =>
        OptionT.fromOption(callTransitions.get(q -> in)).semiflatMap { case CallTransition(q1, p, upd) =>
          update(env.widen, upd).map(env => (q1, (p, env.stackify) :: stack, env.call))
        }
      case Tag.Return() =>
        stack match {
          case (p, env1) :: stack =>
            OptionT.fromOption(returnTransitions.get((q, p, in))).semiflatMap { case ReturnTransition(q1, upd) =>
              update(env.merge(env1), upd).map((q1, stack, _))
            }
          case Nil =>
            OptionT.liftF(
              F.raiseError(STTException("inconsistent stack state. Input is probably not a well-formed tree")))
        }
    }

  def eval0(env: Env[Variable.Normal, Out], e: Expr0[Out]): F[Chain[Out]] = {
    def loop(e: Expr0[Out], acc: Chain[Out]): F[Chain[Out]] = {
      e match {
        case Expr0.Empty() =>
          F.pure(acc)
        case Expr0.Var(x) =>
          env.lookup[F, Expr0[Out]](x).flatMap(loop(_, acc))
        case Expr0.Char(c) =>
          F.pure(acc.append(c))
        case Expr0.Subtree(open, sub, close) =>
          loop(sub, acc.append(open)).map(_.append(close))
        case Expr0.Concat(left, right) =>
          for {
            acc <- loop(left, acc)
            acc <- loop(right, acc)
          } yield acc
        case Expr0.Subst(inner, arg) =>
          loop(inner.subst(arg), acc)
      }
    }
    loop(e, Chain.empty)
  }

  def apply(s: Stream[F, In]): Stream[F, Out] = {
    def go(chunk: Chunk[In],
           idx: Int,
           rest: Stream[F, In],
           state: Int,
           stack: List[(StackElem, Env[Variable.Stack, Out])],
           env: Env[Variable.Normal, Out],
           lastKnownFinal: Option[(Int, Env[Variable.Normal, Out])],
           accSinceLastFinal: Chain[In],
           chunkAcc: Chain[Out]): Pull[F, Out, Unit] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.chain(chunkAcc)) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            go(hd, 0, tl, state, stack, env, lastKnownFinal, accSinceLastFinal, Chain.empty)
          case None =>
            // we are at the end of the input
            // did we reach a final state?
            lastKnownFinal match {
              case Some((state, env)) =>
                // we did reach a final state, emit the outputs from the last one reached
                // and push back the input read since into the stream, then proceed
                Pull
                  .eval(eval0(env, finalStates(state)))
                  .flatMap(outs =>
                    go(Chunk.chain(accSinceLastFinal),
                       0,
                       Stream.empty,
                       initial,
                       Nil,
                       Env.create(variables),
                       None,
                       Chain.empty,
                       chunkAcc ++ outs))
              case None =>
                // we did not reach a final state, do we have leftover inputs?
                if (accSinceLastFinal.isEmpty) {
                  // no we don't, everything has been processed, stop here
                  Pull.done
                } else {
                  // we do have unprocessed inputs, this is an error
                  Pull.raiseError(STTException("malformed input"))
                }
            }
        }
      } else {
        val in = chunk(idx)
        Pull
          // try to step with the current input character
          .eval(step(state, stack, env)(in).value)
          .flatMap {
            case Some((state, stack, env)) if isFinal(state) =>
              // we can step and the target state is final,
              // register this as the last encountered final state,
              // reinitialize the input buffer to empty, and proceed
              // consume the input symbol
              go(chunk, idx + 1, rest, state, stack, env, (state, env).some, Chain.empty, chunkAcc)
            case Some((state, stack, env)) =>
              // we can step and the target state is NOT final,
              // add the just read input into the buffer of read
              // inputs since last final state, and proceed
              go(chunk, idx + 1, rest, state, stack, env, lastKnownFinal, accSinceLastFinal.append(in), chunkAcc)
            case None =>
              // we cannot step from this state, is it final?
              finalStates.get(state) match {
                case Some(finalExpr) if accSinceLastFinal.nonEmpty =>
                  // it is a final state, emit the associated output, reset buffer,
                  // reset to initial state, and proceed without consuming the input
                  Pull
                    .eval(eval0(env, finalExpr))
                    .flatMap(outs =>
                      go(chunk, idx, rest, initial, Nil, Env.create(variables), None, Chain.empty, chunkAcc ++ outs))
                case _ =>
                  // it is not a final state
                  lastKnownFinal match {
                    case Some((state, env)) =>
                      // we reached a final state before, let's emit what should have been emitted
                      // there, and push the input buffer back to the stream,
                      // as well as unconsumed current chunk
                      Pull
                        .eval(eval0(env, finalStates(state)))
                        .flatMap(outs =>
                          go(Chunk.chain(accSinceLastFinal),
                             0,
                             Stream.chunk(chunk.drop(idx)) ++ rest,
                             initial,
                             Nil,
                             Env.create(variables),
                             None,
                             Chain.empty,
                             chunkAcc ++ outs))
                    case None =>
                      // there is no known final, we will emit nothing and just fail
                      Pull.output(Chunk.chain(chunkAcc)) >> Pull.raiseError(
                        STTException(
                          show"malformed input, prefix ${(accSinceLastFinal :+ in).mkString_(", ")} is not accepted"))
                  }
              }
          }
      }

    go(Chunk.empty, 0, s, initial, List.empty, Env.create(variables), None, Chain.empty, Chain.empty).stream

  }

}
