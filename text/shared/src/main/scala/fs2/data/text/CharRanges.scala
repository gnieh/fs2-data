/*
 * Copyright 2024 fs2-data Project
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

package fs2.data.text

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

sealed abstract class CharRanges {
  @tailrec
  final def contains(c: Char): Boolean =
    this match {
      case CharRanges.Empty                       => false
      case CharRanges.Node(min, max, left, right) =>
        if (min <= c && c <= max)
          true
        else if (c < min)
          left.contains(c)
        else
          right.contains(c)
    }

  final def ranges: List[(Char, Char)] =
    this match {
      case CharRanges.Empty                       => Nil
      case CharRanges.Node(min, max, left, right) => left.ranges ++ List((min, max)) ++ right.ranges
    }

  final def union(that: CharRanges): CharRanges =
    CharRanges.fromRanges(this.ranges reverse_::: that.ranges)

}

object CharRanges {
  private case class Node(min: Char, max: Char, left: CharRanges, right: CharRanges) extends CharRanges
  private case object Empty extends CharRanges

  def fromRanges(ranges: (Char, Char)*): CharRanges =
    fromRanges(ranges.toList)

  def fromRanges(ranges: List[(Char, Char)]): CharRanges = {
    val sorted = ranges
      // make sure elements are valid intervals
      .map {
        case (m1, m2) if m1 > m2 => (m2, m1)
        case i                   => i
      }
      // sort by lower bound
      .sortBy(_._1)
    // merge touching or overlapping intervals
    @tailrec
    def merge(current: (Char, Char),
              ranges: List[(Char, Char)],
              acc: VectorBuilder[(Char, Char)]): Vector[(Char, Char)] =
      ranges match {
        case Nil                  => (acc += current).result()
        case (min, max) :: ranges =>
          if (min <= current._2 + 1)
            merge((current._1, max), ranges, acc)
          else
            merge((min, max), ranges, acc += current)
      }

    sorted match {
      case Nil      => Empty
      case hd :: tl =>
        val merged = merge(hd, tl, new VectorBuilder)
        // now we try to keep the tree somewhat balanced, by taking the middle interval as root,
        // and repeat the process for left and right sub trees
        def makeTree(low: Int, high: Int): CharRanges =
          if (low > high)
            Empty
          else {
            val mid = (high + low) / 2
            val (min, max) = merged(mid)
            Node(min, max, makeTree(low, mid - 1), makeTree(mid + 1, high))
          }
        makeTree(0, merged.size - 1)
    }
  }

}
