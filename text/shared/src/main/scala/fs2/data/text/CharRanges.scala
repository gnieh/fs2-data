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

import cats._
import cats.syntax.order._
import cats.syntax.show._
import scala.collection.immutable.VectorBuilder
import scala.annotation.tailrec
import cats.data.NonEmptyVector

/** Disjoint ranges of characters. */
sealed abstract class CharRanges {
  import CharRanges._

  /** Indicates whether a character is included in on of the ranges. */
  def contains(c: Char): Boolean =
    this match {
      case Empty           => false
      case All             => true
      case Single(range)   => range.contains(c)
      case Several(ranges) => search(ranges, c)
    }

  /** Merges these ranges with some other ones. */
  def merge(that: CharRanges): CharRanges =
    (this, that) match {
      case (All, _) | (_, All)          => All
      case (Empty, _)                   => that
      case (_, Empty)                   => this
      case (Single(r1), Single(r2))     => mergeRanges(r1, r2)
      case (Single(r), Several(rs))     => insert(r, rs)
      case (Several(rs), Single(r))     => insert(r, rs)
      case (Several(rs1), Several(rs2)) => insertAll(rs1, rs2)
    }

  /** Inserts one range in these character ranges.
    * It fails in case the new range is not valid (lower bound stricly greater than upper bound)
    */
  def insert(range: CharRange): CharRanges =
    this match {
      case Empty           => Single(range)
      case All             => All
      case Single(r)       => mergeRanges(range, r)
      case Several(ranges) => insert(range, ranges)
    }

  /** Adds a single character into these ranges. */
  def insert(c: Char): CharRanges =
    insert(CharRange.of(c))

  def isEmpty: Boolean = this == Empty

  def isAll: Boolean = this == All

  def ranges: List[CharRange] =
    this match {
      case Empty         => Nil
      case All           => List(CharRange.unsafeOf(Char.MinValue, Char.MaxValue))
      case Single(range) => List(range)
      case Several(rs)   => rs.toVector.toList
    }

  // ranges are sorted, use binary search
  private def search(ranges: NonEmptyVector[CharRange], c: Char): Boolean = {
    def loop(low: Int, high: Int): Boolean =
      if (low > high) {
        false
      } else {
        val mid = (low + high) / 2
        val range = ranges.getUnsafe(mid)
        if (range.contains(c))
          true
        else if (range.low > c)
          loop(low, mid - 1)
        else
          loop(mid + 1, high)
      }
    loop(0, ranges.length - 1)
  }

  @tailrec
  private def loop(idx: Int,
                   rs: NonEmptyVector[CharRange],
                   current: CharRange,
                   acc: VectorBuilder[CharRange]): NonEmptyVector[CharRange] =
    if (idx >= rs.length) { NonEmptyVector.fromVectorUnsafe(acc.addOne(current).result()) }
    else {
      val r = rs.getUnsafe(idx)
      if (r.overlapsOrTouches(current))
        // merge both ranges and continues
        loop(idx + 1,
             rs,
             CharRange.unsafeOf(math.min(r.low, current.low).toChar, math.max(r.high, current.high).toChar),
             acc)
      else if (r < current)
        // the range we want to insert comes strictly after the current one, accumulate and continue
        loop(idx + 1, rs, current, acc.addOne(r))
      else
        // next range is strictly after the one we want to insert, insert our range, and accumulate rest, we are done
        NonEmptyVector.fromVectorUnsafe(acc.addOne(current).addAll(rs.toVector.drop(idx)).result())
    }

  private def insert(r: CharRange, rs: NonEmptyVector[CharRange]): CharRanges = {
    val ranges = loop(0, rs, r, new VectorBuilder)
    if (ranges.length == 1)
      CharRanges.Single(ranges.getUnsafe(0))
    else
      CharRanges.Several(ranges)
  }

  private def insertAll(rs1: NonEmptyVector[CharRange], rs2: NonEmptyVector[CharRange]): CharRanges = {
    val ranges = rs1.foldLeft(rs2)(loop(0, _, _, new VectorBuilder))
    if (ranges.length == 1)
      CharRanges.Single(ranges.getUnsafe(0))
    else
      CharRanges.Several(ranges)
  }

}

object CharRanges {

  /** Conatins no characters at all. */
  def empty: CharRanges = Empty

  /** Contains all possible characters. */
  def all: CharRanges = All

  /** Creates character ranges, failing if a range is invalid (lower bound strictly higher than upper bound).
    * Ranges are inclusive.
    */
  def of(fst: CharRange, rest: CharRange*): CharRanges =
    rest.foldLeft[CharRanges](Single(fst))(_.insert(_))

  private case object Empty extends CharRanges

  private case object All extends CharRanges

  private case class Single(range: CharRange) extends CharRanges

  // invariant: ranges are sorted by increasing low cna don't overlap or touch
  private case class Several(rs: NonEmptyVector[CharRange]) extends CharRanges

  private def mergeRanges(r1: CharRange, r2: CharRange): CharRanges =
    if (r1.overlapsOrTouches(r2)) {
      CharRanges.Single(CharRange.unsafeOf(math.min(r1.low, r2.low).toChar, math.max(r1.high, r2.high).toChar))
    } else {
      // disjoint ranges
      val first = Order.min(r1, r2)
      val second = Order.max(r1, r2)
      CharRanges.Several(NonEmptyVector.of(first, second))
    }

  implicit val show: Show[CharRanges] = Show.show {
    case Empty       => "<empty>"
    case All         => "<any char>"
    case Single(r)   => r.show
    case Several(rs) => show"{$rs}"
  }
  implicit val eq: Eq[CharRanges] = Eq.instance {
    case (Empty, Empty)               => true
    case (All, All)                   => true
    case (Single(r1), Single(r2))     => r1 === r2
    case (Several(rs1), Several(rs2)) => rs1 === rs2
    case _                            => false
  }
}

class CharRange private (val low: Char, val high: Char) {
  def contains(c: Char): Boolean =
    c >= low && c <= high
  def overlapsOrTouches(that: CharRange): Boolean =
    this.high >= that.low && this.low <= that.high
}

object CharRange {
  implicit val order: Order[CharRange] = Order.by(_.low)
  implicit val show: Show[CharRange] = Show.show(r => s"[${r.low}-${r.high}]")
  implicit val eq: Eq[CharRange] = Eq.instance((r1, r2) => r1.low == r2.low && r1.high == r2.high)

  def of(c: Char): CharRange =
    new CharRange(c, c)

  def of(range: (Char, Char)): Either[String, CharRange] =
    if (range._1 > range._2)
      Left(s"Invalid character range (${range._1} > ${range._2})")
    else
      Right(new CharRange(range._1, range._2))

  def unsafeOf(range: (Char, Char)): CharRange =
    new CharRange(range._1, range._2)
}
