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

import scodec.bits.BitVector
import scala.annotation.tailrec

sealed trait PathTree {

  /** Indicates whether this path tree has a unique path. */
  def hasUniquePath: Boolean = {
    @tailrec
    def loop(tree: PathTree): Boolean =
      tree match {
        case PathTree.Leaf(_)         => true
        case PathTree.Binary(_, _, _) => false
        case PathTree.Unary(_, child) => loop(child)
      }
    loop(this)
  }

  /** Returns the leaves in lexicographical order of the paths leading to them. */
  def leaves: List[(BitVector, State)] = {
    // TODO is it worth making it tail recursive?
    def loop(tree: PathTree, acc: BitVector): List[(BitVector, State)] =
      tree match {
        case PathTree.Leaf(q)             => List(acc -> q)
        case PathTree.Unary(_, child)     => loop(child, acc)
        case PathTree.Binary(_, fst, snd) => loop(fst, acc :+ false) ++ loop(snd, acc :+ true)
      }
    loop(this, BitVector.empty)
  }

  /** Prunes the leaf at the lowest binary node.
    * If no such nodes exists, then returns `None`.
    */
  def prune(path: BitVector): Option[PathTree] = {
    def loop(tree: PathTree, path: BitVector): PathTree =
      tree match {
        case PathTree.Leaf(_)             => tree // leaf is not in the tree, unchanged
        case PathTree.Unary(q, child)     => PathTree.Unary(q, loop(child, path))
        case PathTree.Binary(q, fst, snd) =>
          // we ensure that `loop` is never called with an empty path
          val choice = path(0)
          if (path.size == 1)
            // this is the lowest branching, remove its
            PathTree.Unary(q, if (choice) snd else fst)
          else
            // continue in the chosen branch and prune it
            PathTree.Binary(q, if (choice) loop(fst, path.tail) else fst, if (!choice) loop(snd, path.tail) else snd)
      }
    if (path.isEmpty)
      None
    else
      Some(loop(this, path))
  }

  def replaceLeaf(path: BitVector, sub: PathTree): Option[PathTree] = {
    def loop(tree: PathTree, path: BitVector): Option[PathTree] =
      tree match {
        case PathTree.Leaf(_) if path.isEmpty =>
          // we reached the leaf to replaced
          Some(sub)
        case PathTree.Unary(state, child) =>
          loop(child, path).map { child => PathTree.Unary(state, child) }
        case PathTree.Binary(state, fst, snd) if path.size >= 1L =>
          if (path(0))
            loop(fst, path.tail).map(fst => PathTree.Binary(state, fst, snd))
          else
            loop(snd, path.tail).map(snd => PathTree.Binary(state, fst, snd))
        case _ =>
          // this path doesn't represent any leaf in the path tree
          None
      }
    loop(this, path)
  }
}

object PathTree {
  case class Leaf(state: State) extends PathTree

  case class Unary(state: State, child: PathTree) extends PathTree

  case class Binary(state: State, first: PathTree, second: PathTree) extends PathTree
}
