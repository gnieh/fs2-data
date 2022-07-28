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
package json

import ast.Builder
import jsonpath.internals._
import pfsa.{PDFA, PNFA}

import cats.effect.Concurrent
import cats.syntax.all._

package object jsonpath {

  /** Namespace containing the various JsonPath filtering pipes. */
  def filter[F[_]]: PartiallyAppliedFilter[F] = new PartiallyAppliedFilter(true)

  /** Namespace containing the various JsonPath filtering pipes. */
  final class PartiallyAppliedFilter[F[_]] private[jsonpath] (val dummy: Boolean) extends AnyVal {

    /** Selects all macthing elements in the input stream. Each matching element is emitted in a new stream.
      * Matching is performed in a streaming fashion, and events are emitted as early as possible.
      * The match streams are emitted in the same order they are encountered in the input stream, i.e.
      * in the order of the opening tags matching the query.
      *
      * '''Warning''': make sure you actually consume all the emitted streams otherwise
      * this can lead to memory problems.
      */
    def raw(path: JsonPath)(implicit F: Concurrent[F]): Pipe[F, Token, Stream[F, Token]] =
      _.through(JsonTagger.pipe)
        .through(new JsonQueryPipe(compileJsonPath(path)).raw(_))
        .map(_.map(untag(_)).unNone)

    /** Selects the first match in the input stream. The tokens of the first matching
      * value are emitted as they are read.
      *
      * The other matches are gently discarded.
      */
    def first(path: JsonPath)(implicit F: Concurrent[F]): Pipe[F, Token, Token] =
      _.through(JsonTagger.pipe)
        .through(new JsonQueryPipe(compileJsonPath(path)).first(_))
        .map(untag(_))
        .unNone

    /** Selects all matching elements in the input stream, and builds an AST.
      *
      * If `ordered` is set to `false` (`true` by default), built elements are emitted as soon
      * as possible (i.e. when the value is entirely built).
      */
    def values[T](path: JsonPath, ordered: Boolean = true)(implicit
        F: Concurrent[F],
        builder: Builder[T]): Pipe[F, Token, T] =
      _.through(JsonTagger.pipe)
        .through(new JsonQueryPipe(compileJsonPath(path))
          .aggregate(_, _.map(untag(_)).unNone.through(json.ast.values).compile.toList, ordered))
        .flatMap(Stream.emits(_))

    /** Selects all matching elements in the input stream, and applies the [[fs2.Collector]] to it.
      *
      * If `ordered` is set to `false` (`true` by default), built elements are emitted as soon
      * as possible (i.e. when the value is entirely built).
      */
    def collect[T](path: JsonPath, collector: Collector.Aux[Token, T], ordered: Boolean = true)(implicit
        F: Concurrent[F]): Pipe[F, Token, T] =
      _.through(JsonTagger.pipe)
        .through(
          new JsonQueryPipe(compileJsonPath(path))
            .aggregate(_, _.map(untag(_)).unNone.compile.to(collector), ordered))

  }

  private def untag(tj: TaggedJson): Option[Token] =
    tj match {
      case TaggedJson.Raw(t)                 => Some(t)
      case TaggedJson.StartArrayElement(_)   => None
      case TaggedJson.EndArrayElement        => None
      case TaggedJson.StartObjectValue(name) => Some(Token.Key(name))
      case TaggedJson.EndObjectValue         => None
    }

  private def compileJsonPath(path: JsonPath): PDFA[PathMatcher, TaggedJson] = {

    def makeKey(p: Property): PathMatcher =
      p match {
        case Property.Name(n)  => PathMatcher.Key(n)
        case Property.Wildcard => PathMatcher.AnyKey
      }

    def loop(locations: List[Location],
             q: Int,
             acc: Map[Int, List[(Option[PathMatcher], Int)]]): (Int, Map[Int, List[(Option[PathMatcher], Int)]]) =
      locations match {
        case Nil => (q, acc)
        case Location.Child(name) :: locations =>
          loop(locations, q + 1, acc.combine(Map(q -> List(makeKey(name).some -> (q + 1)))))
        case Location.Descendent(name) :: locations =>
          loop(locations,
               q + 1,
               acc.combine(Map(q -> List(PathMatcher.AnyKey.some -> q, makeKey(name).some -> (q + 1)))))
        case Location.Pred(p) :: locations =>
          p match {
            case Predicate.Wildcard | Predicate.Range(0, None) =>
              loop(locations,
                   q + 1,
                   acc.combine(
                     Map(q -> List(PathMatcher.Range(0, Int.MaxValue).some -> (q + 1)),
                         (q + 1) -> List(PathMatcher.Range(0, Int.MaxValue).some -> (q + 1)))))
            case Predicate.Index(idx) =>
              loop(
                locations,
                q + 1,
                acc.combine(
                  Map(q -> List(PathMatcher.Range(0, idx - 1).some -> q, PathMatcher.Range(idx, idx).some -> (q + 1)))))
            case Predicate.Range(low, None) =>
              loop(locations,
                   q + 1,
                   acc.combine(
                     Map(q -> List(PathMatcher.Range(0, low - 1).some -> q,
                                   PathMatcher.Range(low, Int.MaxValue).some -> (q + 1)))))
            case Predicate.Range(low, Some(high)) =>
              loop(
                locations,
                q + 1,
                acc.combine(
                  Map(
                    q -> List(PathMatcher.Range(0, low - 1).some -> q, PathMatcher.Range(low, high).some -> (q + 1)))))
          }
      }
    val (f, transitions) = loop(path.locations.toList, 0, Map.empty)

    val nfa = new PNFA(0, Set(f), transitions)
    val dfa = nfa.determinize
    dfa
  }

}
