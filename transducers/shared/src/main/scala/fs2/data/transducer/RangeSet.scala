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
import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.kernel.BoundedEnumerable
import cats.syntax.all._

import scala.collection.compat.immutable.LazyList
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

/** A set of ranges for some enumerable type. */
sealed trait RangeSet[T] {

  /** Indicates whether this set of ranges contains the given character. */
  def contains(c: T): Boolean

  /** Returns the index of the given character in this set of ranges, or `-1`
    * if it is not contained.
    */
  def indexOf(c: T): Int

  /** Returns the character at the given index in this set of ranges,
    * or `None` if idex is out of bounds.
    */
  def charAt(idx: Int): Option[T]

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

  /** Returns the number of character in this set of ranges. */
  def size: Int

  /** Indicates whether `this` overlaps with `that`.
    * Returns `true` iif the exists `t`, such that
    * `this.contains(t) && that.contains(t)`
    */
  def overlap(that: RangeSet[T]): Boolean

}

object RangeSet {

  /** The empty set of character ranges */
  def empty[T: BoundedEnumerable: Integral]: RangeSet[T] = Empty()

  /** The set that contains all characters */
  def all[T: BoundedEnumerable: Integral]: RangeSet[T] = All()

  /** Creates a singleton set of a singleton range */
  def char[T: BoundedEnumerable: Integral](c: T): RangeSet[T] =
    Ranges(NonEmptyVector.one(Range(c, c)), false)

  /** Creates a set of ranges based on the provided single characters */
  def chars[T: BoundedEnumerable: Integral](c1: T, c2: T, cs: T*): RangeSet[T] =
    ranges((c1, c1), (c2, c2), cs.map(c => (c, c)): _*)

  /** Creates a singleton set of ranges */
  def range[T](r: (T, T))(implicit T: BoundedEnumerable[T], I: Integral[T]): RangeSet[T] = {
    implicit val order = T.order
    val lower = r._1.min(r._2)
    val upper = r._1.max(r._2)
    if (lower == Char.MinValue && upper == Char.MaxValue)
      All()
    else
      Ranges(NonEmptyVector.one(Range(lower, upper)), false)
  }

  /** Creates a set of ranges */
  def ranges[T](r1: (T, T), r2: (T, T), rs: (T, T)*)(implicit T: BoundedEnumerable[T], I: Integral[T]): RangeSet[T] = {
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

  private case class Range[T](lower: T, upper: T) {
    def contains(c: T)(implicit T: BoundedEnumerable[T]): Boolean = {
      implicit val order: Order[T] = T.order
      lower <= c && upper >= c
    }
    def indexOf(c: T)(implicit T: BoundedEnumerable[T], I: Integral[T]): Int =
      if (contains(c))
        I.toInt(I.minus(c, lower))
      else
        -1
    def at(idx: Int)(implicit I: Integral[T]): Option[T] = {
      if (idx < 0 || idx >= size)
        None
      else
        I.plus(lower, I.fromInt(idx)).some
    }
    def overlapsOrAdjacent(that: Range[T])(implicit T: BoundedEnumerable[T]): Boolean = {
      implicit val order: Order[T] = T.order
      this.upper >= T.cyclePrevious(that.lower) && this.lower <= T.cycleNext(that.upper)
    }
    def merge(that: Range[T])(implicit T: BoundedEnumerable[T]): Range[T] = {
      implicit val order: Order[T] = T.order
      Range(this.lower.min(that.lower), this.upper.max(that.upper))
    }
    def enumerate(implicit T: BoundedEnumerable[T]): LazyList[T] = {
      implicit val order: Order[T] = T.order
      LazyList.iterate(lower)(T.cycleNext(_)).takeWhile(t => t >= lower && t <= upper)
    }
    def size(implicit I: Integral[T]): Int =
      I.toInt(I.minus(upper, lower)) + 1
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

  private case class All[T]()(implicit T: BoundedEnumerable[T], I: Integral[T]) extends RangeSet[T] {
    def contains(c: T): Boolean = true
    def indexOf(c: T): Int =
      I.toInt(I.minus(c, T.minBound))
    def charAt(idx: Int): Option[T] =
      if (idx < 0 || idx >= size)
        None
      else
        I.plus(T.minBound, I.fromInt(idx)).some
    def invert: RangeSet[T] = Empty()
    def enumerate: LazyList[T] = LazyList.from(T.membersAscending)
    def min: Option[T] = T.minBound.some
    def max: Option[T] = T.maxBound.some
    def overlap(that: RangeSet[T]): Boolean = that != Empty()
    def isEmpty: Boolean = false
    def size: Int = I.toInt(I.minus(T.maxBound, T.minBound)) + 1
  }

  private case class Empty[T: BoundedEnumerable: Integral]() extends RangeSet[T] {
    def contains(c: T): Boolean = false
    def indexOf(c: T): Int = -1
    def charAt(idx: Int): Option[T] = None
    def invert: RangeSet[T] = All()
    def enumerate: LazyList[T] = LazyList.empty
    def min: Option[T] = None
    def max: Option[T] = None
    def overlap(that: RangeSet[T]): Boolean = false
    def isEmpty: Boolean = true
    def size: Int = 0
  }

  private case class Ranges[T](ranges: NonEmptyVector[Range[T]], inverted: Boolean)(implicit
      T: BoundedEnumerable[T],
      I: Integral[T])
      extends RangeSet[T] {
    implicit val order: Order[T] = T.order
    // if there is only one range, it does not represent all the values
    // this ensures that the complement cannot be empty
    assert(ranges.length > 1 || ranges.head.lower != T.minBound || ranges.last.upper != T.maxBound)

    private lazy val complement: NonEmptyVector[Range[T]] = {
      def loop(idx: Int, low: T, builder: VectorBuilder[Range[T]]): Vector[Range[T]] =
        ranges.get(idx) match {
          case Some(range) =>
            if (low < range.lower) {
              // the new range lower bound is strictly smaller than the next lower bound
              // fill in the gap
              builder.addOne(Range(low, T.cyclePrevious(range.lower)))
              if (range.upper < T.maxBound)
                loop(idx + 1, T.cycleNext(range.upper), builder)
              else
                // there is no room after for any range, this is the end
                builder.result()
            } else {
              loop(idx + 1, T.cycleNext(range.upper), builder)
            }
          case None =>
            builder.addOne(Range(low, T.maxBound)).result()
        }
      NonEmptyVector.fromVectorUnsafe(loop(0, T.minBound, new VectorBuilder))
    }

    private def search(c: T, ranges: NonEmptyVector[Range[T]]): Int = {
      @tailrec
      def loop(low: Int, high: Int): Int =
        if (low > high) {
          -1
        } else {
          val mid = (low + high) / 2
          val range = ranges.getUnsafe(mid)
          val idx = range.indexOf(c)
          if (idx >= 0)
            ranges.iterator.map(_.size).take(mid).sum + idx
          else if (c < range.lower)
            loop(low, mid - 1)
          else
            loop(mid + 1, high)
        }
      loop(0, ranges.length - 1)
    }

    private def at(idx: Int, ranges: NonEmptyVector[Range[T]]): Option[T] = {
      if (idx < 0 || idx >= size) {
        None
      } else {
        @tailrec
        def loop(ridx: Int, idx: Int): Option[T] =
          if (ridx >= ranges.length) {
            None
          } else {
            val range = ranges.getUnsafe(ridx)
            val rangeSize = range.size
            if (idx < rangeSize)
              range.at(idx)
            else
              loop(ridx + 1, idx - rangeSize)
          }
        loop(0, idx)
      }
    }

    def contains(c: T): Boolean =
      search(c, ranges) != -1 ^ inverted
    def indexOf(c: T): Int =
      if (inverted) {
        // search in the complement
        search(c, complement)
      } else {
        // search in the ranges
        search(c, ranges)
      }
    def charAt(idx: Int): Option[T] =
      if (inverted)
        at(idx, complement)
      else
        at(idx, ranges)
    def invert: RangeSet[T] = copy(inverted = !inverted)
    def enumerate: LazyList[T] =
      if (inverted)
        LazyList.from(complement.iterator).flatMap(_.enumerate)
      else
        LazyList.from(ranges.iterator).flatMap(_.enumerate)
    def min: Option[T] = ranges.head.lower.some
    def max: Option[T] = ranges.last.upper.some
    def overlap(that: RangeSet[T]): Boolean =
      that match {
        case All()   => true
        case Empty() => false
        case _       => enumerate.exists(that.contains(_))
      }
    def isEmpty: Boolean = false
    def size: Int =
      if (inverted)
        complement.map(_.size).sumAll
      else
        ranges.map(_.size).sumAll
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
