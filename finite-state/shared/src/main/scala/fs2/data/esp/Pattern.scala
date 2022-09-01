/*
 * Copyright 2019-2022 Lucas Satabin
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
}

object Pattern {

  /** Matches anything. */
  case object Wildcard extends Pattern[Nothing]

  /** Matches the end of stream. */
  case object EOS extends Pattern[Nothing]

  /** Matches the input in a state at a depth for some input pattern. */
  case class Input[Tag](q: Option[Int], d: Option[Int], inner: Pattern[Tag]) extends Pattern[Tag]

  /** Matches some open tag. */
  case class Open[Tag](tag: Option[Tag], as: Option[String]) extends Pattern[Tag]

  /** Matches some close tag. */
  case class Close[Tag](tag: Option[Tag], as: Option[String]) extends Pattern[Tag]

  /** Matches some leaf value. */
  case class Leaf[Tag](value: Option[Tag], as: Option[String]) extends Pattern[Tag]

  /** Alternative, matched from left to right. */
  case class Or[Tag](patterns: NonEmptyChain[Pattern[Tag]]) extends Pattern[Tag]

  implicit def PatternIsPattern[T]: IsPattern[Pattern[T], Tag[T]] =
    new IsPattern[Pattern[T], Tag[T]] {

      override def decompose(pat: Pattern[T]): List[Skeleton[Tag[T]]] =
        pat match {
          case Wildcard =>
            List(Skeleton.Wildcard(none))
          case EOS =>
            List(Skeleton.noArgConstructor(Tag.End))
          case Input(Some(q), Some(d), inner) =>
            decompose(inner).map { inner =>
              Skeleton.Constructor[Tag[T]](
                Tag.Input,
                List(Skeleton.noArgConstructor(Tag.State(q)), Skeleton.noArgConstructor(Tag.Depth(d)), inner))
            }
          case Input(Some(q), None, inner) =>
            decompose(inner).map { inner =>
              Skeleton.Constructor[Tag[T]](
                Tag.Input,
                List(Skeleton.noArgConstructor(Tag.State(q)), Skeleton.Wildcard(none), inner))
            }
          case Input(None, Some(d), inner) =>
            decompose(inner).map { inner =>
              Skeleton.Constructor[Tag[T]](
                Tag.Input,
                List(Skeleton.Wildcard(none), Skeleton.noArgConstructor(Tag.Depth(d)), inner))
            }
          case Input(None, None, inner) =>
            decompose(inner).map { inner =>
              Skeleton.Constructor[Tag[T]](Tag.Input, List(Skeleton.Wildcard(none), Skeleton.Wildcard(none), inner))
            }
          case Open(None, as) =>
            List(Skeleton.Constructor(Tag.Open, List(Skeleton.Wildcard(as))))
          case Open(Some(tag), Some(as)) =>
            List(Skeleton.As(Skeleton.Constructor(Tag.Open, List(Skeleton.noArgConstructor(Tag.Name(tag)))), as))
          case Open(Some(tag), None) =>
            List(Skeleton.Constructor(Tag.Open, List(Skeleton.noArgConstructor(Tag.Name(tag)))))
          case Close(None, as) =>
            List(Skeleton.Constructor[Tag[T]](Tag.Close, List(Skeleton.Wildcard(as))))
          case Close(Some(tag), Some(as)) =>
            List(Skeleton.As(Skeleton.Constructor(Tag.Close, List(Skeleton.noArgConstructor(Tag.Name(tag)))), as))
          case Close(Some(tag), None) =>
            List(Skeleton.Constructor(Tag.Close, List(Skeleton.noArgConstructor(Tag.Name(tag)))))
          case Leaf(None, as) =>
            List(Skeleton.Constructor[Tag[T]](Tag.Leaf, List(Skeleton.Wildcard(as))))
          case Leaf(Some(v), Some(as)) =>
            List(Skeleton.As(Skeleton.Constructor(Tag.Leaf, List(Skeleton.noArgConstructor(Tag.Value(v)))), as))
          case Leaf(Some(v), None) =>
            List(Skeleton.Constructor(Tag.Leaf, List(Skeleton.noArgConstructor(Tag.Value(v)))))
          case Or(alts) =>
            alts.foldMap(decompose(_))
        }

    }

  def heuristic[T]: Heuristic[Tag[T]] =
    Heuristic.sequence(List(Heuristic.firstRow, Heuristic.smallDefault))

}

class PatternDsl[Tag] {

  def state(q: Int)(inner: Pattern[Tag]): Pattern[Tag] =
    Pattern.Input(q.some, none, inner)

  def state(q: Int, d: Int)(inner: Pattern[Tag]): Pattern[Tag] =
    Pattern.Input(q.some, d.some, inner)

  def value(v: Tag, as: Option[String] = none): Pattern[Tag] =
    Pattern.Leaf(v.some, as)

  def value(as: String): Pattern[Tag] =
    Pattern.Leaf(none, as.some)

  val value: Pattern[Tag] =
    Pattern.Leaf(none, none)

  val eos: Pattern[Tag] =
    Pattern.EOS

  val __ : Pattern[Tag] =
    Pattern.Wildcard

  def open(t: Tag, as: Option[String] = none): Pattern[Tag] =
    Pattern.Open(t.some, as)

  def open(as: String): Pattern[Tag] =
    Pattern.Open(none, as.some)

  val open: Pattern[Tag] =
    Pattern.Open(none, none)

  def close(t: Tag, as: Option[String] = none): Pattern[Tag] =
    Pattern.Close(t.some, as)

  def close(as: String): Pattern[Tag] =
    Pattern.Close(none, as.some)

  val close: Pattern[Tag] =
    Pattern.Close(none, none)

}
