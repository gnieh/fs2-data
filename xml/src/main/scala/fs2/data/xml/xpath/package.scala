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
import automaton.{PDFA, PNFA, Pred}
import Pred.syntax._

import cats.effect.Concurrent

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
      new XmlQueryPipe(compileXPath(path))

    /** Selects all matching elements in the input stream, and builds an element DOM.
      * Built elements are emitted as soon as possible (i.e. when the closing tag is found).
      */
    def dom[T](path: XPath)(implicit F: Concurrent[F], builder: ElementBuilder.Aux[T]): Pipe[F, XmlEvent, T] =
      _.through(raw(path))
        .parEvalMapUnordered(Int.MaxValue)(m => m.through(xml.dom.elements[F, T]).compile.toList)
        .flatMap(Stream.emits(_))

    /** Selects all matching elements in the input stream, and applies the [[fs2.Collector]] to it.
      * Built elements are emitted as soon as possible (i.e. when the closing tag is found).
      */
    def collect[T](path: XPath, collector: Collector.Aux[XmlEvent, T])(implicit
        F: Concurrent[F]): Pipe[F, XmlEvent, T] =
      _.through(raw(path))
        .parEvalMapUnordered(Int.MaxValue)(_.compile.to(collector))

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

    val transitions =
      path.locations.zipWithIndex.foldLeft(Map.empty[Int, List[(Option[LocationMatch], Int)]]) {
        case (acc, (l @ Location(axis, _, _), idx)) =>
          axis match {
            case Axis.Child => acc.updated(idx, List((Some(makeLocation(l)), idx + 1)))
            case Axis.Descendent =>
              acc.updated(idx, List((Some(makeLocation(l)), idx + 1), (Some(LocationMatch.True), idx)))
          }
      }
    new PNFA(0, Set(transitions.size), transitions).determinize
  }

}
