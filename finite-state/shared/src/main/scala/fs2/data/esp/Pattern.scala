/*
 * Copyright 2022 Lucas Satabin
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

import pattern._

import cats.data.NonEmptyChain
import cats.syntax.all._

/** A pattern to be matched.
  * A pattern can capture a submatch, that is then usable in the RHS.
  */
sealed trait Pattern[+Tag] {
  def |[Tag1 >: Tag](that: Pattern[Tag1]): Pattern[Tag1] =
    (this, that) match {
      case (Pattern.Wildcard, _) | (_, Pattern.Wildcard) => Pattern.Wildcard
      case (Pattern.Or(alts1), Pattern.Or(alts2))        => Pattern.Or(alts1 ++ alts2)
      case (_, _)                                        => Pattern.Or(NonEmptyChain(this, that))
    }

  def when[Tag1 >: Tag](guard: Guard[Tag1]): Pattern[Tag1] =
    this match {
      case Pattern.Guarded(p, g) => Pattern.Guarded(p, Guard.And(g.widen[Tag1], guard))
      case _                     => Pattern.Guarded(this, guard)
    }
}

object Pattern {

  /** Matches anything. */
  case object Wildcard extends Pattern[Nothing]

  /** Matches the end of stream. */
  case object EOS extends Pattern[Nothing]

  /** Matches the input in a state at a depth for some input pattern. */
  case class Input[Tag](q: Option[Int], d: Option[Int], inner: Pattern[Tag]) extends Pattern[Tag]

  /** Matches some open tag. */
  case class Open[Tag](tag: Option[Tag]) extends Pattern[Tag]

  /** Matches some close tag. */
  case class Close[Tag](tag: Option[Tag]) extends Pattern[Tag]

  /** Matches some leaf value. */
  case class Leaf[Tag](value: Option[Tag]) extends Pattern[Tag]

  /** Alternative, matched from left to right. */
  case class Or[Tag](patterns: NonEmptyChain[Pattern[Tag]]) extends Pattern[Tag]

  /** A guarded pattern. */
  case class Guarded[Tag](inner: Pattern[Tag], guard: Guard[Tag]) extends Pattern[Tag]

  implicit def PatternIsPattern[T]: IsPattern[Pattern[T], Guard[T], Tag[T]] =
    new IsPattern[Pattern[T], Guard[T], Tag[T]] {

      override val trueTag: Tag[T] = Tag.True

      override def decompose(pat: Pattern[T]): List[RawSkeleton[Guard[T], Tag[T]]] =
        decompose(pat, None)

      private def decompose(pat: Pattern[T], guard: Option[Guard[T]]): List[RawSkeleton[Guard[T], Tag[T]]] =
        pat match {
          case Wildcard =>
            List(RawSkeleton.Wildcard[Guard[T], Tag[T]](guard))
          case EOS =>
            List(RawSkeleton.Constructor[Guard[T], Tag[T]](Tag.End, Nil, guard))
          case Input(Some(q), Some(d), inner) =>
            decompose(inner).map { inner =>
              RawSkeleton.Constructor[Guard[T], Tag[T]](
                Tag.Input,
                List(RawSkeleton.noArgConstructor(Tag.State(q)), RawSkeleton.noArgConstructor(Tag.Depth(d)), inner),
                guard)
            }
          case Input(Some(q), None, inner) =>
            decompose(inner).map { inner =>
              RawSkeleton.Constructor[Guard[T], Tag[T]](
                Tag.Input,
                List(RawSkeleton.noArgConstructor(Tag.State(q)), RawSkeleton.Wildcard(none), inner),
                guard)
            }
          case Input(None, Some(d), inner) =>
            decompose(inner).map { inner =>
              RawSkeleton.Constructor[Guard[T], Tag[T]](
                Tag.Input,
                List(RawSkeleton.wildcard, RawSkeleton.noArgConstructor(Tag.Depth(d)), inner),
                guard)
            }
          case Input(None, None, inner) =>
            decompose(inner).map { inner =>
              RawSkeleton
                .Constructor[Guard[T], Tag[T]](Tag.Input,
                                               List(RawSkeleton.wildcard, RawSkeleton.wildcard, inner),
                                               guard)
            }
          case Open(None) =>
            List(RawSkeleton.Constructor[Guard[T], Tag[T]](Tag.Open, List(RawSkeleton.wildcard), guard))
          case Open(Some(tag)) =>
            List(
              RawSkeleton
                .Constructor[Guard[T], Tag[T]](Tag.Open, List(RawSkeleton.noArgConstructor(Tag.Name(tag))), guard))
          case Close(None) =>
            List(RawSkeleton.Constructor[Guard[T], Tag[T]](Tag.Close, List(RawSkeleton.wildcard), guard))
          case Close(Some(tag)) =>
            List(
              RawSkeleton
                .Constructor[Guard[T], Tag[T]](Tag.Close, List(RawSkeleton.noArgConstructor(Tag.Name(tag))), guard))
          case Leaf(None) =>
            List(RawSkeleton.Constructor[Guard[T], Tag[T]](Tag.Leaf, List(RawSkeleton.wildcard), guard))
          case Leaf(Some(v)) =>
            List(
              RawSkeleton
                .Constructor[Guard[T], Tag[T]](Tag.Leaf, List(RawSkeleton.noArgConstructor(Tag.Value(v))), guard))
          case Or(alts) =>
            alts.foldMap(decompose(_, guard))
          case Guarded(p, g) =>
            decompose(p, guard.fold(Some(g.widen[T]))(g1 => Some(Guard.And(g1, g.widen[T]))))
        }

    }

}

class PatternDsl[Tag] {

  def state(q: Int)(inner: Pattern[Tag]): Pattern[Tag] =
    Pattern.Input(q.some, none, inner)

  def state(q: Int, d: Int)(inner: Pattern[Tag]): Pattern[Tag] =
    Pattern.Input(q.some, d.some, inner)

  def value(v: Tag): Pattern[Tag] =
    Pattern.Leaf(v.some)

  val value: Pattern[Tag] =
    Pattern.Leaf(none)

  val eos: Pattern[Tag] =
    Pattern.EOS

  val any: Pattern[Tag] =
    Pattern.Wildcard

  def open(t: Tag): Pattern[Tag] =
    Pattern.Open(t.some)

  val open: Pattern[Tag] =
    Pattern.Open(none)

  def close(t: Tag): Pattern[Tag] =
    Pattern.Close(t.some)

  val close: Pattern[Tag] =
    Pattern.Close(none)

  def in[Tag](t: Tag, ts: Tag*): Guard[Tag] =
    Guard.In((t +: ts).toSet)

  def notIn[Tag](t: Tag, ts: Tag*): Guard[Tag] =
    Guard.NotIn((t +: ts).toSet)

}
