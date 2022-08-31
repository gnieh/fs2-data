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
import mft.Conversion

import cats.syntax.option._

sealed trait Pattern[+T] {
  def |[U >: T](that: Pattern[U]): Pattern[U] =
    (this, that) match {
      case (Pattern.Wildcard, _) | (_, Pattern.Wildcard) => Pattern.Wildcard
      case (_, _)                                        => Pattern.Or(this, that)
    }
}
object Pattern {
  case object Wildcard extends Pattern[Nothing]
  case object EOS extends Pattern[Nothing]
  case class State[T](q: Option[Int], d: Option[Int], inner: Pattern[T]) extends Pattern[T]
  case class Open(name: Option[String]) extends Pattern[Nothing]
  case class Close(name: Option[String]) extends Pattern[Nothing]
  case class Leaf[T](v: T) extends Pattern[T]
  case class Or[T](left: Pattern[T], right: Pattern[T]) extends Pattern[T]

  def dsl[Tag, T]: PatternDsl[Tag, T] =
    new PatternDsl[Tag, T]

  implicit def PatIsPattern[T]: IsPattern[Pattern[T], Tag[T]] =
    new IsPattern[Pattern[T], Tag[T]] {

      override def decompose(pat: Pattern[T]): List[Skeleton[Tag[T]]] =
        pat match {
          case Wildcard =>
            List(Skeleton.Wildcard(None))
          case EOS =>
            List(Skeleton.Constructor(Tag.End, Nil))
          case State(Some(q), None, inner) =>
            decompose(inner).map { inner =>
              Skeleton.Constructor(Tag.Input,
                                   List(Skeleton.Constructor(Tag.State(q), Nil), Skeleton.Wildcard(None), inner))
            }
          case State(None, None, inner) =>
            decompose(inner).map { inner =>
              Skeleton.Constructor(Tag.Input, List(Skeleton.Wildcard(None), Skeleton.Wildcard(None), inner))
            }
          case State(Some(q), Some(d), inner) =>
            decompose(inner).map { inner =>
              Skeleton.Constructor(
                Tag.Input,
                List(Skeleton.Constructor(Tag.State(q), Nil), Skeleton.Constructor(Tag.Depth(d), Nil), inner))
            }
          case State(None, Some(d), inner) =>
            decompose(inner).map { inner =>
              Skeleton.Constructor(Tag.Input,
                                   List(Skeleton.Wildcard(None), Skeleton.Constructor(Tag.Depth(d), Nil), inner))
            }
          case Open(n) =>
            List(
              Skeleton.Constructor(
                Tag.Open,
                List(n.map(n => Skeleton.Constructor(Tag.Name(n), Nil)).getOrElse(Skeleton.Wildcard(None)))
              ))
          case Close(n) =>
            List(
              Skeleton.Constructor(
                Tag.Close,
                List(n.map(n => Skeleton.Constructor(Tag.Name(n), Nil)).getOrElse(Skeleton.Wildcard(None)))
              ))
          case Leaf(v) =>
            List(Skeleton.Constructor(Tag.Value(v), Nil))
          case Or(l, r) =>
            decompose(l) ++ decompose(r)
        }

    }

  def heuristic[T]: Heuristic[Tag[T]] =
    Heuristic.sequence(List(Heuristic.firstRow, Heuristic.smallDefault))

}

class PatternDsl[Tag, T] {

  def state(q: Int)(inner: Pattern[T]): Pattern[T] =
    Pattern.State(q.some, none, inner)

  def state(q: Int, d: Int)(inner: Pattern[T]): Pattern[T] =
    Pattern.State(q.some, d.some, inner)

  def value(v: T): Pattern[T] =
    Pattern.Leaf(v)

  val eos: Pattern[T] =
    Pattern.EOS

  val __ : Pattern[T] =
    Pattern.Wildcard

  def open(t: Tag)(implicit T: Conversion[Tag, _]): Pattern[T] =
    Pattern.Open(T.makeName(t).some)

  val open: Pattern[T] =
    Pattern.Open(none)

  def close(t: Tag)(implicit T: Conversion[Tag, _]): Pattern[T] =
    Pattern.Close(T.makeName(t).some)

  val close: Pattern[T] =
    Pattern.Close(none)

}
