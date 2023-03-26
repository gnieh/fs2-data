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

package fs2
package data
package json
package jsonpath
package internals

import pfsa._

import cats.effect.Concurrent
import cats.syntax.all._

private[jsonpath] sealed trait PathMatcher
private[jsonpath] object PathMatcher {
  case object True extends PathMatcher
  case object False extends PathMatcher
  case object AnyKey extends PathMatcher
  case class Key(name: String) extends PathMatcher
  case class Range(low: Int, high: Int) extends PathMatcher {
    def intersects(that: Range): Boolean =
      this.high >= that.low && this.low <= that.high

    def intersection(that: Range): Range =
      Range(Math.max(this.low, that.low), Math.min(this.high, that.high))

  }

  case class And(left: PathMatcher, right: PathMatcher) extends PathMatcher
  case class Or(left: PathMatcher, right: PathMatcher) extends PathMatcher
  case class Not(inner: PathMatcher) extends PathMatcher

  implicit val PathMatcherPred: Pred[PathMatcher, TaggedJson] =
    new Pred[PathMatcher, TaggedJson] {

      override def satsifies(p: PathMatcher)(e: TaggedJson): Boolean =
        (p, e) match {
          case (True, _) => true
          case (Range(low, high), TaggedJson.StartArrayElement(idx)) =>
            idx >= low && idx <= high
          case (AnyKey, TaggedJson.StartObjectValue(_))   => true
          case (Key(n1), TaggedJson.StartObjectValue(n2)) => n1 === n2
          case (And(l, r), _)                             => satsifies(l)(e) && satsifies(r)(e)
          case (Or(l, r), _)                              => satsifies(l)(e) || satsifies(r)(e)
          case (Not(i), _)                                => !satsifies(i)(e)
          case (_, _)                                     => false
        }

      override def always: PathMatcher = True

      override def never: PathMatcher = False

      override def and(p1: PathMatcher, p2: PathMatcher): PathMatcher =
        (p1, p2) match {
          case (True, _)                            => p2
          case (_, True)                            => p1
          case (False, _)                           => False
          case (_, False)                           => False
          case (Key(_), AnyKey)                     => p1
          case (AnyKey, Key(_))                     => p2
          case (AnyKey, Not(Key(_)))                => p2
          case (Not(Key(_)), AnyKey)                => p1
          case (Key(n1), Key(n2))                   => if (n1 === n2) p1 else False
          case (Key(n1), Not(Key(n2)))              => if (n1 === n2) False else p1
          case (Not(Key(n1)), Key(n2))              => if (n1 === n2) False else p2
          case (Not(AnyKey), Key(_))                => False
          case (Key(_), Not(AnyKey))                => False
          case (r1 @ Range(_, _), r2 @ Range(_, _)) => if (r1.intersects(r2)) r1.intersection(r2) else False
          case (r1 @ Range(_, _), Not(r2 @ Range(_, _))) if !r1.intersects(r2) => r1
          case (Not(r1 @ Range(_, _)), r2 @ Range(_, _)) if !r1.intersects(r2) => r2
          case (Key(_) | AnyKey, Range(_, _))                                  => False
          case (Range(_, _), Key(_) | AnyKey)                                  => False
          case (_, _)                                                          => And(p1, p2)
        }

      override def or(p1: PathMatcher, p2: PathMatcher): PathMatcher =
        (p1, p2) match {
          case (True, _)                 => True
          case (_, True)                 => True
          case (False, _)                => p2
          case (_, False)                => p1
          case (Key(_) | AnyKey, AnyKey) => AnyKey
          case (AnyKey, Key(_) | AnyKey) => AnyKey
          case (_, _)                    => Or(p1, p2)
        }

      override def not(p: PathMatcher): PathMatcher =
        p match {
          case Not(p) => p
          case True   => False
          case False  => True
          case _      => Not(p)
        }

      override def isSatisfiable(p: PathMatcher): Boolean =
        p match {
          case False       => false
          case Range(l, h) => l <= h
          case _           => true
        }

    }
}

private[jsonpath] class JsonQueryPipe[F[_]: Concurrent](dfa: PDFA[PathMatcher, TaggedJson])
    extends TreeQueryPipe[F, TaggedJson, TaggedJson, PathMatcher, TaggedJson](dfa) {

  override val emitOpenAndClose = false

  override def makeMatchingElement(tok: TaggedJson): TaggedJson =
    tok

  override def isOpen(tok: TaggedJson) =
    tok match {
      case TaggedJson.StartObjectValue(_) | TaggedJson.StartArrayElement(_) =>
        tok.some
      case _ => none
    }

  override def isClose(tok: TaggedJson) =
    tok match {
      case TaggedJson.EndObjectValue | TaggedJson.EndArrayElement =>
        true
      case _ => false
    }

}
