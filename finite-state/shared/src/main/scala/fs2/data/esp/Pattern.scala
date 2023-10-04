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

import pattern._

import cats.data.NonEmptyChain
import cats.syntax.all._

/** A pattern to be matched.
  * A pattern can capture a submatch, that is then usable in the RHS.
  */
sealed trait Pattern[Guard, T] {
  def |(that: Pattern[Guard, T]): Pattern[Guard, T] =
    (this, that) match {
      case (Pattern.Wildcard(), _) | (_, Pattern.Wildcard()) => Pattern.Wildcard()
      case (Pattern.Or(alts1), Pattern.Or(alts2))            => Pattern.Or(alts1 ++ alts2)
      case (_, _)                                            => Pattern.Or(NonEmptyChain(this, that))
    }

  def when(guard: Guard): Pattern[Guard, T] =
    Pattern.Guarded(this, guard)
}

object Pattern {

  /** Matches anything. */
  case class Wildcard[Guard, T]() extends Pattern[Guard, T]

  /** Matches the end of stream. */
  case class EOS[Guard, T]() extends Pattern[Guard, T]

  /** Matches the input in a state at a depth for some input pattern. */
  case class Input[Guard, T](q: Option[Int], d: Option[Int], inner: Pattern[Guard, T]) extends Pattern[Guard, T]

  /** Matches some open tag. */
  case class Open[Guard, T](tag: Option[T]) extends Pattern[Guard, T]

  /** Matches some close tag. */
  case class Close[Guard, T](tag: Option[T]) extends Pattern[Guard, T]

  /** Matches some leaf value. */
  case class Leaf[Guard, T](value: Option[T]) extends Pattern[Guard, T]

  /** Alternative, matched from left to right. */
  case class Or[Guard, T](patterns: NonEmptyChain[Pattern[Guard, T]]) extends Pattern[Guard, T]

  /** A guarded pattern. */
  case class Guarded[Guard, T](inner: Pattern[Guard, T], guard: Guard) extends Pattern[Guard, T]

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

class PatternDsl[Guard, T] {

  def state(q: Int)(inner: Pattern[Guard, T]): Pattern[Guard, T] =
    Pattern.Input(q.some, none, inner)

  def state(q: Int, d: Int)(inner: Pattern[Guard, T]): Pattern[Guard, T] =
    Pattern.Input(q.some, d.some, inner)

  def value(v: T): Pattern[Guard, T] =
    Pattern.Leaf(v.some)

  val value: Pattern[Guard, T] =
    Pattern.Leaf(none)

  val eos: Pattern[Guard, T] =
    Pattern.EOS()

  val any: Pattern[Guard, T] =
    Pattern.Wildcard()

  def open(t: T): Pattern[Guard, T] =
    Pattern.Open(t.some)

  val open: Pattern[Guard, T] =
    Pattern.Open(none)

  def close(t: T): Pattern[Guard, T] =
    Pattern.Close(t.some)

  val close: Pattern[Guard, T] =
    Pattern.Close(none)

}
