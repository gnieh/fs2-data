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

package fs2.data.transducer

import cats.Order
import cats.syntax.all._
import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.kernel.BoundedEnumerable

/** A set of ranges for some enumerable type. */
sealed trait RangeSet[T] {

  /** Indicates whether this set of ranges contains the given character. */
  def contains(c: T): Boolean

  /** Inverts this set of character ranges.
    * Forall c, this.contains(c) == !this.invert.contains(c)
    */
  def invert: RangeSet[T]

  /** Enumerates all characters in this set of ranges, in ascending order. */
  def enumerate: LazyList[T]

  /** Returns the minimal elements in these sets. */
  def min: Option[T]

  /** Returns the maximal elements in these sets. */
  def max: Option[T]

  /** Indicates whether this sets contains no characters. */
  def isEmpty: Boolean

  /** Indicates whether `this` overlaps with `that`.
    * Returns `true` iif the exists `t`, such that
    * `this.contains(t) && that.contains(t)`
    */
  def overlap(that: RangeSet[T]): Boolean

}

object RangeSet {

  /** The empty set of character ranges */
  def empty[T: BoundedEnumerable]: RangeSet[T] = Empty()

  /** The set that contains all characters */
  def all[T: BoundedEnumerable]: RangeSet[T] = All()

  /** Creates a singleton set of a singleton range */
  def char[T: BoundedEnumerable](c: T): RangeSet[T] =
    Ranges(NonEmptyVector.one(Range(c, c)), false)

  /** Creates a set of ranges based on the provided single characters */
  def chars[T: BoundedEnumerable](c1: T, c2: T, cs: T*): RangeSet[T] =
    ranges((c1, c1), (c2, c2), cs.map(c => (c, c)): _*)

  /** Creates a singleton set of ranges */
  def range[T](r: (T, T))(implicit T: BoundedEnumerable[T]): RangeSet[T] = {
    implicit val order = T.order
    val lower = r._1.min(r._2)
    val upper = r._1.max(r._2)
    if (lower == Char.MinValue && upper == Char.MaxValue)
      All()
    else
      Ranges(NonEmptyVector.one(Range(lower, upper)), false)
  }

  /** Creates a set of ranges */
  def ranges[T](r1: (T, T), r2: (T, T), rs: (T, T)*)(implicit T: BoundedEnumerable[T]): RangeSet[T] = {
    implicit val order = T.order
    val ranges =
      NonEmptyList(r1, r2 :: rs.toList)
        .map { case (c1, c2) => Range(c1.min(c2), c1.max(c2)) }
        .sortBy(_.lower)
    def merge(ranges: NonEmptyList[Range[T]]): NonEmptyList[Range[T]] =
      ranges match {
        case NonEmptyList(r1, r2 :: ranges) if r1.overlapsOrAdjacent(r2) => merge(NonEmptyList(r1.merge(r2), ranges))
        case NonEmptyList(r1, r2 :: ranges)                              => r1 :: merge(NonEmptyList(r2, ranges))
        case NonEmptyList(r1, Nil)                                       => NonEmptyList.one(r1)
      }
    merge(ranges) match {
      case NonEmptyList(Range(Char.MinValue, Char.MaxValue), Nil) => All()
      case merged                                                 => Ranges(merged.toNev, false)
    }

  }

  private case class Range[T](lower: T, upper: T)(implicit T: BoundedEnumerable[T]) {
    implicit val order = T.order
    def contains(c: T): Boolean =
      lower <= c && upper >= c
    def overlapsOrAdjacent(that: Range[T]): Boolean =
      this.upper >= T.cyclePrevious(that.lower) && this.lower <= T.cycleNext(that.upper)
    def merge(that: Range[T]): Range[T] =
      Range(this.lower.min(that.lower), this.upper.max(that.upper))
    def enumerate: LazyList[T] =
      LazyList.iterate(lower)(T.cycleNext(_)).takeWhile(_ <= upper)
  }

  private object Range {
    implicit def order[T](implicit T: Order[T]): Order[Range[T]] = Order.from { (x: Range[T], y: Range[T]) =>
      val mincmp = T.compare(x.lower, y.lower)
      if (mincmp == 0)
        T.compare(x.upper, y.upper)
      else
        mincmp

    }
    implicit def show[T: Show]: Show[Range[T]] = Show.show { t =>
      if (t.lower == t.upper)
        t.lower.show
      else
        show"${t.lower}-${t.upper}"
    }
  }

  private case class All[T]()(implicit T: BoundedEnumerable[T]) extends RangeSet[T] {
    def contains(c: T): Boolean = true
    def invert: RangeSet[T] = Empty()
    def enumerate: LazyList[T] = T.membersAscending
    def min: Option[T] = T.minBound.some
    def max: Option[T] = T.maxBound.some
    def overlap(that: RangeSet[T]): Boolean = that != Empty()
    def isEmpty: Boolean = false
  }

  private case class Empty[T: BoundedEnumerable]() extends RangeSet[T] {
    def contains(c: T): Boolean = false
    def invert: RangeSet[T] = All()
    def enumerate: LazyList[T] = LazyList.empty
    def min: Option[T] = None
    def max: Option[T] = None
    def overlap(that: RangeSet[T]): Boolean = false
    def isEmpty: Boolean = true
  }

  private case class Ranges[T](ranges: NonEmptyVector[Range[T]], inverted: Boolean) extends RangeSet[T] {
    implicit val order = ranges.head.order
    def contains(c: T): Boolean = {
      def search(low: Int, high: Int): Boolean =
        if (low > high) {
          inverted
        } else {
          val mid = (low + high) / 2
          val range = ranges.getUnsafe(mid)
          if (range.contains(c))
            !inverted
          else if (c < range.lower)
            search(low, mid - 1)
          else
            search(mid + 1, high)
        }
      search(0, ranges.length - 1)
    }
    def invert: RangeSet[T] = copy(inverted = !inverted)
    def enumerate: LazyList[T] = LazyList.from(ranges.iterator).flatMap(_.enumerate)
    def min: Option[T] = ranges.head.lower.some
    def max: Option[T] = ranges.last.upper.some
    def overlap(that: RangeSet[T]): Boolean =
      that match {
        case All()   => true
        case Empty() => false
        case _       => enumerate.exists(that.contains(_))
      }
    def isEmpty: Boolean = false
  }

  implicit def RangeSetShow[T: Show]: Show[RangeSet[T]] = Show.show {
    case Empty()               => "ε"
    case All()                 => "*"
    case Ranges(ranges, false) => ranges.mkString_("[", "", "]")
    case Ranges(ranges, true)  => ranges.mkString_("[^", "", "]")
  }

  implicit def RangeSetSetLike[T]: SetLike[RangeSet[T], T] =
    new SetLike[RangeSet[T], T] {
      def contains(s: RangeSet[T])(c: T): Boolean = s.contains(c)
    }
}
