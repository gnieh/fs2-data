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

import cats.syntax.all._
import scala.collection.immutable.VectorBuilder
import scodec.bits.BitVector

object Interpreter {

  private def emitChunk[T](chunkAcc: VectorBuilder[T]) =
    Pull.output(Chunk.vector(chunkAcc.result()))

  def pipe[F[_], C](sst: SST)(implicit F: RaiseThrowable[F], C: CharLikeChunks[F, C]): Pipe[F, C, C] = {

    def stepToNextResting(q: State): PathTree =
      sst.transitions.get(q) match {
        case Some(Transitions.Choice(fst, snd)) =>
          PathTree.Binary(q, stepToNextResting(fst.target), stepToNextResting(snd.target))
        case Some(Transitions.Skip(t)) => PathTree.Unary(q, stepToNextResting(t.target))
        case _                         => PathTree.Leaf(q)
      }

    def computePathTree(pt: PathTree, from: List[(BitVector, State)], reachedPreviously: Set[State]) = {
      val newPts =
        from.map { case (path, q) =>
          val pt = stepToNextResting(q)
          path -> pt.leaves.foldLeftM[Option, PathTree](pt) { case (pt, (path, q)) =>
            if (reachedPreviously.contains(q))
              pt.prune(path)
            else
              pt.some
          }
        }
      val updatedPt =
        newPts.foldLeftM[Option, PathTree](pt) {
          case (pt, (path, None)) =>
            // prune the path
            pt.prune(path)
          case (pt, (path, Some(sub))) =>
            // insert the sub tree at the path
            pt.replaceLeaf(path, sub)
        }

      updatedPt.map { updatedPt =>
        val leaves = updatedPt.leaves
        val newReached = reachedPreviously ++ leaves.map(_._2)
        (updatedPt, leaves, newReached)
      }
    }

    def simulate(context: C.Context) = {
      // when this is called, we are on a character that can start the SST
      // first thing is to go to the next resting state and compute the first path tree
      val pt = stepToNextResting(sst.init)
      val leaves = pt.leaves
      val c = C.current(context)
      val (canStep, cantStep) =
        leaves.partition { case (_, q) => sst.step(q, c).isDefined }
      // for every path that cannot step, if it's not leading to a final state, prune it
      val cleaned =
        cantStep.foldLeftM[Option, PathTree](pt) { case (pt, (path, q)) =>
          if (sst.isFinal(q))
            pt.some
          else
            pt.prune(path)
        }
      // now step from leaves and append the sub tree
      cleaned.flatMap(cleaned =>
        canStep.foldLeftM[Option, PathTree](cleaned) { case (pt, (path, q)) =>
          sst.step(q, c).flatMap(res => pt.replaceLeaf(path, stepToNextResting(res.target)))
        })
      ???
    }

    def go(context: C.Context): Pull[F, C, Unit] =
      // skips all chars until one is matching the start of the first one that matches the beginning of the SST
      // unmatched chars are emitted unchanged keeping chunk structure as much as possible
      C.emitWhile(context, !sst.isStart(_)).flatMap {
        case Some(context) => simulate(context)
        case None          => Pull.done
      }

    s => Stream.suspend(Stream.emit(C.create(s))).flatMap(go(_).stream)
  }

}
