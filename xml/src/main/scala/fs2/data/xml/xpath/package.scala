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

package fs2
package data
package xml

import dom.ElementBuilder
import xpath.internals._
import pfsa.{PDFA, PNFA, Pred}
import Pred.syntax._

import cats.effect.Concurrent
import cats.syntax.all._

package object xpath {

  /** Namespace containing the various XPath filtering pipes. */
  def filter[F[_]]: PartiallyAppliedFilter[F] = new PartiallyAppliedFilter(true)

  /** Namespace containing the various XPath filtering pipes. */
  final class PartiallyAppliedFilter[F[_]] private[xpath] (val dummy: Boolean) extends AnyVal {

    /** Selects all macthing elements in the input stream. Each matching element is emitted in a new stream.
      * Matching is performed in a streaming fashion, and events are emitted as early as possible.
      * The match streams are emitted in the same order they are encountered in the input stream, i.e.
      * in the order of the opening tags matching the query.
      *
      * '''Warning''': make sure you actually consume all the emitted streams otherwise
      * this can lead to memory problems.
      */
    def raw(path: XPath)(implicit F: Concurrent[F]): Pipe[F, XmlEvent, Stream[F, XmlEvent]] =
      new XmlQueryPipe(compileXPath(path)).raw(_)

    /** Selects the first match only. First is meant as in: opening tag appears first in the input, no matter the depth.
      * Tokens of the first match are emitted as they are read from the input.
      *
      * Other results are gently discarded.
      */
    def first(path: XPath)(implicit F: Concurrent[F]): Pipe[F, XmlEvent, XmlEvent] =
      new XmlQueryPipe(compileXPath(path)).first(_)

    /** Selects all matching elements in the input stream, and builds an element DOM.
      *
      * If `ordered` is set to false (`true` by default), built elements are emitted as soon
      * as possible (i.e. when the value is entirely built).
      */
    def dom[T](path: XPath, ordered: Boolean = true)(implicit
        F: Concurrent[F],
        builder: ElementBuilder.Aux[T]): Pipe[F, XmlEvent, T] =
      new XmlQueryPipe(compileXPath(path))
        .aggregate(_, _.through(xml.dom.elements).compile.toList, ordered)
        .flatMap(Stream.emits(_))

    /** Selects all matching elements in the input stream, and applies the [[fs2.Collector]] to it.
      *
      * If `ordered` is set to false (`true` by default), built elements are emitted as soon
      * as possible (i.e. when the value is entirely built).
      */
    def collect[T](path: XPath, collector: Collector.Aux[XmlEvent, T], ordered: Boolean = true)(implicit
        F: Concurrent[F]): Pipe[F, XmlEvent, T] =
      new XmlQueryPipe(compileXPath(path)).aggregate(_, _.compile.to(collector), ordered)

  }

  private def compileXPath(path: XPath): PDFA[LocationMatch, StartElement] = {
    def makePredicate(p: Predicate): LocationMatch =
      p match {
        case Predicate.True             => LocationMatch.True
        case Predicate.False            => LocationMatch.False
        case Predicate.Exists(attr)     => LocationMatch.AttrExists(attr)
        case Predicate.Eq(attr, value)  => LocationMatch.AttrEq(attr, value)
        case Predicate.Neq(attr, value) => LocationMatch.AttrNeq(attr, value)
        case Predicate.And(left, right) => makePredicate(left) && makePredicate(right)
        case Predicate.Or(left, right)  => makePredicate(left) || makePredicate(right)
        case Predicate.Not(inner)       => !makePredicate(inner)
      }

    def makeLocation(l: Location): LocationMatch =
      l match {
        case Location(_, n, p) =>
          val node: LocationMatch =
            n match {
              case Node(None, None) => LocationMatch.True
              case _                => LocationMatch.Element(n)
            }
          node && p.map(makePredicate(_)).getOrElse(LocationMatch.True)
      }

    val (transitions, fs) =
      path.locations.foldLeft((Map.empty[Int, List[(Option[LocationMatch], Int)]], Set.empty[Int])) {
        case ((trans, fs), ors) =>
          val (q1, trans1) =
            ors.foldLeft((0, trans)) { case ((q, trans), l @ Location(axis, _, _)) =>
              axis match {
                case Axis.Child => (q + 1, trans.combine(Map((q -> List((Some(makeLocation(l)), q + 1))))))
                case Axis.Descendent =>
                  (q + 1, trans.combine(Map(q -> List((Some(makeLocation(l)), q + 1), (Some(LocationMatch.True), q)))))
              }
            }
          (trans1, fs + q1)

      }
    new PNFA(0, fs, transitions).determinize
  }

}
