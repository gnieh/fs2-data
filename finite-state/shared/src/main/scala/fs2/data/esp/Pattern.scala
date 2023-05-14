/*
 * Copyright 2023 Lucas Satabin
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

package fs2.data
package esp

import cats.data.NonEmptyChain
import cats.syntax.all._
import cats.{Eq, Monoid}

import pattern._

/** A pattern to be matched.
  */
sealed trait Pattern[Guard, Tag] {
  import Pattern._

  def |(that: Pattern[Guard, Tag]): Pattern[Guard, Tag] =
    (this, that) match {
      case (Wildcard(), _) | (_, Wildcard()) => Wildcard()
      case (Or(alts1), Or(alts2))            => Or(alts1 ++ alts2)
      case (_, _)                            => Or(NonEmptyChain(this, that))
    }

  def when(guard: Guard): Pattern[Guard, Tag] =
    Guarded(this, guard)

  private def compatible[T: Eq](o1: Option[T], o2: Option[T]): Boolean =
    (o1, o2).mapN(_ === _).getOrElse(true)

  def and(
      that: Pattern[Guard, Tag])(implicit eqTag: Eq[Tag], combineGuard: Monoid[Guard]): Option[Pattern[Guard, Tag]] =
    (this, that) match {
      case (Input(q1, d1, inner1), Input(q2, d2, inner2)) if compatible(q1, q2) && compatible(d1, d2) =>
        inner1.and(inner2).map(Input(q1.orElse(q2), d1.orElse(d2), _))
      case (Open(t1), Open(t2)) if compatible(t1, t2) =>
        Open(t1.orElse(t2)).some
      case (Close(t1), Close(t2)) if compatible(t1, t2) =>
        Close(t1.orElse(t2)).some
      case (Leaf(t1), Leaf(t2)) if compatible(t1, t2) =>
        Leaf(t1.orElse(t2)).some
      case (Or(alts1), Or(alts2)) =>
        NonEmptyChain
          .fromSeq(alts1.toList.flatMap { p1 =>
            alts2.toList.flatMap { p2 =>
              p1.and(p2)
            }
          })
          .map(alts => if (alts.size === 1) alts.head else Or(alts))
      case (Or(alts), _) =>
        NonEmptyChain
          .fromSeq(alts.toList.flatMap(p => p.and(that)))
          .map(alts => if (alts.size === 1) alts.head else Or(alts))
      case (_, Or(alts)) =>
        NonEmptyChain
          .fromSeq(alts.toList.flatMap(p => p.and(this)))
          .map(alts => if (alts.size === 1) alts.head else Or(alts))
      case (Guarded(inner1, g1), Guarded(inner2, g2)) =>
        inner1.and(inner2).map(Guarded(_, g1.combine(g2)))
      case (Guarded(inner, g), _) =>
        inner.and(that).map(Guarded(_, g))
      case (_, Guarded(inner, g)) =>
        inner.and(this).map(Guarded(_, g))
      case (Wildcard(), _) =>
        that.some
      case (_, Wildcard()) =>
        this.some
      case (EOS(), EOS()) =>
        this.some
      case (_, _) =>
        none
    }

}

object Pattern {

  /** Matches anything. */
  case class Wildcard[Guard, Tag]() extends Pattern[Guard, Tag]

  /** Matches the end of stream. */
  case class EOS[Guard, Tag]() extends Pattern[Guard, Tag]

  /** Matches the input in a state at a depth for some input pattern. */
  case class Input[Guard, Tag](q: Option[Int], d: Option[Int], inner: Pattern[Guard, Tag]) extends Pattern[Guard, Tag]

  /** Matches some open tag. */
  case class Open[Guard, Tag](tag: Option[Tag]) extends Pattern[Guard, Tag]

  /** Matches some close tag. */
  case class Close[Guard, Tag](tag: Option[Tag]) extends Pattern[Guard, Tag]

  /** Matches some leaf value. */
  case class Leaf[Guard, Tag](value: Option[Tag]) extends Pattern[Guard, Tag]

  /** Alternative, matched from left to right. */
  case class Or[Guard, Tag](patterns: NonEmptyChain[Pattern[Guard, Tag]]) extends Pattern[Guard, Tag]

  /** A guarded pattern. */
  case class Guarded[Guard, Tag](inner: Pattern[Guard, Tag], guard: Guard) extends Pattern[Guard, Tag]

  implicit def PatternIsPattern[G, T]: IsPattern[Pattern[G, T], G, Tag[T]] =
    new IsPattern[Pattern[G, T], G, Tag[T]] {

      override val trueTag: Tag[T] = Tag.Open

      override def decompose(pat: Pattern[G, T]): List[RawSkeleton[G, Tag[T]]] =
        decompose(pat, None)

      private def decompose(pat: Pattern[G, T], guard: Option[G]): List[RawSkeleton[G, Tag[T]]] =
        pat match {
          case Wildcard() =>
            List(RawSkeleton.Wildcard[G, Tag[T]](guard))
          case EOS() =>
            List(RawSkeleton.Constructor[G, Tag[T]](Tag.End, Nil, guard))
          case Input(Some(q), Some(d), inner) =>
            decompose(inner).map { inner =>
              RawSkeleton.Constructor[G, Tag[T]](
                Tag.Input,
                List(RawSkeleton.noArgConstructor(Tag.State(q)), RawSkeleton.noArgConstructor(Tag.Depth(d)), inner),
                guard)
            }
          case Input(Some(q), None, inner) =>
            decompose(inner).map { inner =>
              RawSkeleton.Constructor[G, Tag[T]](
                Tag.Input,
                List(RawSkeleton.noArgConstructor(Tag.State(q)), RawSkeleton.Wildcard(none), inner),
                guard)
            }
          case Input(None, Some(d), inner) =>
            decompose(inner).map { inner =>
              RawSkeleton.Constructor[G, Tag[T]](
                Tag.Input,
                List(RawSkeleton.wildcard, RawSkeleton.noArgConstructor(Tag.Depth(d)), inner),
                guard)
            }
          case Input(None, None, inner) =>
            decompose(inner).map { inner =>
              RawSkeleton
                .Constructor[G, Tag[T]](Tag.Input, List(RawSkeleton.wildcard, RawSkeleton.wildcard, inner), guard)
            }
          case Open(None) =>
            List(RawSkeleton.Constructor[G, Tag[T]](Tag.Open, List(RawSkeleton.wildcard), guard))
          case Open(Some(tag)) =>
            List(
              RawSkeleton
                .Constructor[G, Tag[T]](Tag.Open, List(RawSkeleton.noArgConstructor(Tag.Name(tag))), guard))
          case Close(None) =>
            List(RawSkeleton.Constructor[G, Tag[T]](Tag.Close, List(RawSkeleton.wildcard), guard))
          case Close(Some(tag)) =>
            List(
              RawSkeleton
                .Constructor[G, Tag[T]](Tag.Close, List(RawSkeleton.noArgConstructor(Tag.Name(tag))), guard))
          case Leaf(None) =>
            List(RawSkeleton.Constructor[G, Tag[T]](Tag.Leaf, List(RawSkeleton.wildcard), guard))
          case Leaf(Some(v)) =>
            List(
              RawSkeleton
                .Constructor[G, Tag[T]](Tag.Leaf, List(RawSkeleton.noArgConstructor(Tag.Value(v))), guard))
          case Or(alts) =>
            alts.foldMap(decompose(_, guard))
          case Guarded(p, g) =>
            decompose(p, Some(g))
        }

    }

}

class PatternDsl[Guard, Tag] {

  def state(q: Int)(inner: Pattern[Guard, Tag]): Pattern[Guard, Tag] =
    Pattern.Input(q.some, none, inner)

  def state(q: Int, d: Int)(inner: Pattern[Guard, Tag]): Pattern[Guard, Tag] =
    Pattern.Input(q.some, d.some, inner)

  def value(v: Tag): Pattern[Guard, Tag] =
    Pattern.Leaf(v.some)

  val value: Pattern[Guard, Tag] =
    Pattern.Leaf(none)

  val eos: Pattern[Guard, Tag] =
    Pattern.EOS()

  val any: Pattern[Guard, Tag] =
    Pattern.Wildcard()

  def open(t: Tag): Pattern[Guard, Tag] =
    Pattern.Open(t.some)

  val open: Pattern[Guard, Tag] =
    Pattern.Open(none)

  def close(t: Tag): Pattern[Guard, Tag] =
    Pattern.Close(t.some)

  val close: Pattern[Guard, Tag] =
    Pattern.Close(none)

}
