/*
 * Copyright 2024 fs2-data Project
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
package mft
package query

import cats.Eq
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all._
import weaver._

import esp.{Conversion, Tag}
import pattern.{ConstructorTree, Evaluator}
import pfsa.{Candidate, Pred, Regular}

object OptimizedQuerySpec extends QuerySpec(50)

object NonOptimizedQuerySpec extends QuerySpec(0)

abstract class QuerySpec(credit: Int) extends SimpleIOSuite {

  implicit object StringConversions extends Conversion[String, MiniXML] {

    override def makeOpen(t: String): MiniXML = MiniXML.Open(t)

    override def makeClose(t: String): MiniXML = MiniXML.Close(t)

    override def makeLeaf(t: String): MiniXML = MiniXML.Text(t)

  }

  implicit object evaluator extends Evaluator[NonEmptyList[Set[String]], Tag[String]] {

    override def eval(guard: NonEmptyList[Set[String]], tree: ConstructorTree[Tag[String]]): Option[Tag[String]] =
      tree match {
        case ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(n), _))) if guard.forall(_.contains(n)) =>
          Some(Tag.Open)
        case ConstructorTree(Tag.Name(n), _) if guard.forall(_.contains(n)) => Some(Tag.Open)
        case _                                                              => None
      }

  }

  object MiniXQueryCompiler extends QueryCompiler[String, String, MiniXPath] {

    type Matcher = Set[String]
    type Char = String
    type Pattern = Option[String]
    type Guard = Set[String]

    override implicit object predicate extends Pred[Matcher, Char] {

      override def satisfies(p: Matcher)(e: Char): Boolean = p.contains(e)

      override val always: Matcher = Set("a", "b", "c", "d", "doc")

      override val never: Matcher = Set()

      override def and(p1: Matcher, p2: Matcher): Matcher = p1.intersect(p2)

      override def or(p1: Matcher, p2: Matcher): Matcher = p1.union(p2)

      override def not(p: Matcher): Matcher = always.diff(p)

      override def isSatisfiable(p: Matcher): Boolean = p.nonEmpty

    }

    override implicit object candidate extends Candidate[Matcher, Char] {

      override def pick(set: Matcher): Option[Char] = set.headOption

    }

    override implicit def charsEq: Eq[Matcher] = Eq.fromUniversalEquals

    override def path2regular(path: MiniXPath): Regular[Matcher] =
      path.steps.foldLeft(Regular.epsilon[Matcher]) {
        case (acc, Step.Child(Some(name))) =>
          acc ~ Regular.chars(Set(name))
        case (acc, Step.Child(None)) =>
          acc ~ Regular.any
        case (acc, Step.Descendant(Some(name))) =>
          acc ~ Regular.any.rep ~ Regular.chars(Set(name))
        case (acc, Step.Descendant(None)) =>
          acc ~ Regular.any.rep ~ Regular.any
      }

    override def cases(matcher: Matcher): List[(Pattern, List[Guard])] =
      if (matcher.isEmpty)
        Nil
      else if (matcher.size == 1)
        List(matcher.headOption -> Nil)
      else
        List(None -> List(matcher))

    override def tagOf(pattern: Pattern): Option[String] = pattern

  }

  test("child path") {
    MiniXQueryCompiler
      .compile(Query.Ordpath(MiniXPath(NonEmptyList.one(Step.Child(Some("a")))), None), credit)
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              // format: off
              MiniXML.Open("a"),
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                  MiniXML.Close("a"),
                MiniXML.Close("a"),
                MiniXML.Open("b"),
                  MiniXML.Open("a"),
                  MiniXML.Close("a"),
                MiniXML.Close("b"),
              MiniXML.Close("a"),
              // format: on
            )
          )
          .through(esp.pipe[MiniXML, MiniXML])
          .compile
          .toList
          .map { events =>
            expect.eql(
              List(
                // format: off
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
                // format: on
              ),
              events
            )
          }
      }
  }

  test("any child path") {
    MiniXQueryCompiler
      .compile(Query.Ordpath(MiniXPath(NonEmptyList.one(Step.Child(None))), None), credit)
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              // format: off
              MiniXML.Open("a"),
                MiniXML.Open("a"),
                MiniXML.Close("a"),
                MiniXML.Open("b"),
                MiniXML.Close("b"),
              MiniXML.Close("a"),
              MiniXML.Open("a"),
                MiniXML.Open("c"),
                MiniXML.Close("c"),
              MiniXML.Close("a"),
              // format: on
            )
          )
          .through(esp.pipe[MiniXML, MiniXML])
          .compile
          .toList
          .map { events =>
            expect.eql(
              List(
                // format: off
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                  MiniXML.Open("c"),
                  MiniXML.Close("c"),
                MiniXML.Close("a"),
                // format: on
              ),
              events
            )
          }
      }
  }

  test("descendant path") {
    MiniXQueryCompiler
      .compile(Query.Ordpath(MiniXPath(NonEmptyList.one(Step.Descendant(Some("a")))), None), credit)
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              // format: off
              MiniXML.Open("doc"),
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
              MiniXML.Close("doc"),
              // format: on
            )
          )
          .through(esp.pipe[MiniXML, MiniXML])
          .compile
          .toList
          .map { events =>
            expect.eql(
              List(
                // format: off
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                  MiniXML.Close("a"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                MiniXML.Close("a"),
                // format: on
              ),
              events
            )
          }
      }
  }

  test("any descendant path") {
    MiniXQueryCompiler
      .compile(Query.Ordpath(MiniXPath(NonEmptyList.one(Step.Descendant(None))), None), credit)
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              // format: off
              MiniXML.Open("a"),
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                  MiniXML.Close("a"),
                MiniXML.Close("a"),
                MiniXML.Open("b"),
                  MiniXML.Open("a"),
                  MiniXML.Close("a"),
                MiniXML.Close("b"),
              MiniXML.Close("a"),
              // format: on
            )
          )
          .through(esp.pipe[MiniXML, MiniXML])
          .compile
          .toList
          .map { events =>
            expect.eql(
              List(
                // format: off
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                  MiniXML.Close("a"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                MiniXML.Close("a"),
                MiniXML.Open("b"),
                  MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("b"),
                MiniXML.Open("a"),
                MiniXML.Close("a"),
                // format: on
              ),
              events
            )
          }
      }
  }

  test("simple let") {
    MiniXQueryCompiler
      .compile(Query
                 .LetClause("v",
                            Query.Ordpath(MiniXPath(NonEmptyList.one(Step.Descendant(Some("a")))), None),
                            Query.Variable("v")),
               credit)
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              // format: off
              MiniXML.Open("doc"),
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
              MiniXML.Close("doc"),
              // format: on
            )
          )
          .through(esp.pipe[MiniXML, MiniXML])
          .compile
          .toList
          .map { events =>
            expect.eql(
              List(
                // format: off
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                  MiniXML.Close("a"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                MiniXML.Close("a"),
                // format: on
              ),
              events
            )
          }
      }
  }

  test("simple for") {
    IO(
      MiniXQueryCompiler
        .compile(Query
                   .ForClause("v", MiniXPath(NonEmptyList.one(Step.Descendant(Some("a")))), Query.Variable("v")),
                 credit))
      .flatMap(_.esp[IO])
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              // format: off
              MiniXML.Open("doc"),
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
              MiniXML.Close("doc"),
              // format: on
            )
          )
          .through(esp.pipe[MiniXML, MiniXML])
          .compile
          .toList
          .map { events =>
            expect.eql(
              List(
                // format: off
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("a"),
                    MiniXML.Close("a"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                  MiniXML.Open("a"),
                  MiniXML.Close("a"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                MiniXML.Close("a"),
                MiniXML.Open("a"),
                MiniXML.Close("a"),
                // format: on
              ),
              events
            )
          }
      }
  }

  test("nested for") {
    IO(
      MiniXQueryCompiler
        .compile(
          Query.ForClause(
            "a",
            MiniXPath(NonEmptyList.one(Step.Descendant(Some("a")))),
            Query.ForClause("b",
                            MiniXPath(NonEmptyList.one(Step.Child(None))),
                            Query.Sequence(NonEmptyList.of(Query.Variable("a"), Query.Variable("b"))))
          ),
          credit
        ))
      .flatMap(_.esp[IO])
      .flatMap { esp =>
        Stream
          .emits(List[MiniXML](
              // format: off
              MiniXML.Open("doc"),
                MiniXML.Open("a"),
                  MiniXML.Open("b"),
                  MiniXML.Close("b"),
                  MiniXML.Open("c"),
                  MiniXML.Close("c"),
                MiniXML.Close("a"),
              MiniXML.Close("doc"),
              // format: on
            ))
          .through(esp.pipe[MiniXML, MiniXML])
          .compile
          .toList
          .map { events =>
            expect.eql(
              List(
                // format: off
                MiniXML.Open("a"),
                  MiniXML.Open("b"),
                  MiniXML.Close("b"),
                  MiniXML.Open("c"),
                  MiniXML.Close("c"),
                MiniXML.Close("a"),
                MiniXML.Open("b"),
                MiniXML.Close("b"),
                MiniXML.Open("a"),
                  MiniXML.Open("b"),
                  MiniXML.Close("b"),
                  MiniXML.Open("c"),
                  MiniXML.Close("c"),
                MiniXML.Close("a"),
                MiniXML.Open("c"),
                MiniXML.Close("c"),
                // format: on
              ),
              events
            )
          }
      }
  }

  test("leaves") {
    MiniXQueryCompiler
      .compile(
        Query.ForClause("a", MiniXPath(NonEmptyList.of(Step.Child(Some("a")), Step.Child(None))), Query.Variable("a")),
        credit)
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              //format: off
              MiniXML.Open("a"),
                MiniXML.Open("b"),
                  MiniXML.Text("b text"),
                MiniXML.Close("b"),
                MiniXML.Text("a text"),
              MiniXML.Close("a"),
              //format: on
            )
          )
          .through(esp.pipe)
          .compile
          .toList
          .map(events =>
            expect.same(List[MiniXML](
                          //format: off
                          MiniXML.Open("b"),
                            MiniXML.Text("b text"),
                          MiniXML.Close("b")
                          //format: on
                        ),
                        events))
      }
  }

  test("leaf function") {
    MiniXQueryCompiler
      .compile(Query.ForClause("a", MiniXPath(NonEmptyList.of(Step.Descendant(None))), Query.LeafFunction(_.asRight)),
               credit)
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              //format: off
              MiniXML.Open("a"),
                MiniXML.Open("b"),
                  MiniXML.Text("b text"),
                MiniXML.Close("b"),
                MiniXML.Text("a text"),
              MiniXML.Close("a"),
              //format: on
            )
          )
          .through(esp.pipe)
          .compile
          .toList
          .map(events =>
            expect.same(List[MiniXML](
                          //format: off
                          MiniXML.Text("a text"),
                          MiniXML.Text("b text"),
                          //format: on
                        ),
                        events))
      }
  }

  test("leaf function failure") {
    MiniXQueryCompiler
      .compile(Query.ForClause("a",
                               MiniXPath(NonEmptyList.of(Step.Descendant(None))),
                               Query.LeafFunction(t => s"No text expected but got $t".asLeft)),
               credit)
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              //format: off
              MiniXML.Open("a"),
                MiniXML.Open("b"),
                  MiniXML.Text("b text"),
                MiniXML.Close("b"),
                MiniXML.Text("a text"),
              MiniXML.Close("a"),
              //format: on
            )
          )
          .through(esp.pipe)
          .compile
          .drain
          .as(failure("The pipe should have failed"))
          .handleError(t => expect.same("No text expected but got b text", t.getMessage))
      }
  }

  test("wrap") {
    MiniXQueryCompiler
      .compile(Query
                 .ForClause("a",
                            MiniXPath(NonEmptyList.of(Step.Descendant(None))),
                            Query.Node("wrapped", Query.Variable("a"))),
               credit)
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              //format: off
              MiniXML.Open("a"),
                MiniXML.Open("b"),
                  MiniXML.Text("b text"),
                MiniXML.Close("b"),
                MiniXML.Text("a text"),
              MiniXML.Close("a"),
              //format: on
            )
          )
          .through(esp.pipe)
          .compile
          .toList
          .map(events =>
            expect.same(
              List(
                //format: off
                MiniXML.Open("wrapped"),
                  MiniXML.Open("a"),
                    MiniXML.Open("b"),
                      MiniXML.Text("b text"),
                    MiniXML.Close("b"),
                    MiniXML.Text("a text"),
                  MiniXML.Close("a"),
                MiniXML.Close("wrapped"),
                MiniXML.Open("wrapped"),
                  MiniXML.Open("b"),
                    MiniXML.Text("b text"),
                  MiniXML.Close("b"),
                MiniXML.Close("wrapped"),
                //format: on
              ),
              events
            ))
      }
  }

  test("more nested") {
    val mft =
      MiniXQueryCompiler
        .compile(
          Query.ForClause(
            "v1",
            MiniXPath(NonEmptyList.one(Step.Descendant(Some("a")))),
            Query.ForClause(
              "v2",
              MiniXPath(NonEmptyList.one(Step.Descendant(Some("b")))),
              Query.LetClause(
                "v3",
                Query.Ordpath(MiniXPath(NonEmptyList.one(Step.Descendant(Some("c")))), None),
                Query.LetClause(
                  "v4",
                  Query.Ordpath(MiniXPath(NonEmptyList.one(Step.Descendant(Some("d")))), None),
                  Query.Sequence(NonEmptyList
                    .of(Query.Variable("v1"), Query.Variable("v2"), Query.Variable("v3"), Query.Variable("v4")))
                )
              )
            )
          ),
          credit
        )

    mft
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              // format: off
              MiniXML.Open("doc"),
                MiniXML.Open("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("c"),
                      MiniXML.Open("c"),
                      MiniXML.Close("c"),
                    MiniXML.Close("c"),
                    MiniXML.Open("d"),
                    MiniXML.Close("d"),
                    MiniXML.Open("d"),
                    MiniXML.Close("d"),
                  MiniXML.Close("b"),
                  MiniXML.Open("b"),
                    MiniXML.Open("d"),
                    MiniXML.Close("d"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
              MiniXML.Close("doc"),
              // format: on
            )
          )
          .through(esp.pipe)
          .compile
          .toList
          .map(events =>
            expect.same(
              List[MiniXML](
                // format: off
                MiniXML.Open("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("c"),
                      MiniXML.Open("c"),
                      MiniXML.Close("c"),
                    MiniXML.Close("c"),
                    MiniXML.Open("d"),
                    MiniXML.Close("d"),
                    MiniXML.Open("d"),
                    MiniXML.Close("d"),
                  MiniXML.Close("b"),
                  MiniXML.Open("b"),
                    MiniXML.Open("d"),
                    MiniXML.Close("d"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
                MiniXML.Open("b"),
                  MiniXML.Open("c"),
                    MiniXML.Open("c"),
                    MiniXML.Close("c"),
                  MiniXML.Close("c"),
                  MiniXML.Open("d"),
                  MiniXML.Close("d"),
                  MiniXML.Open("d"),
                  MiniXML.Close("d"),
                MiniXML.Close("b"),
                MiniXML.Open("c"),
                  MiniXML.Open("c"),
                  MiniXML.Close("c"),
                MiniXML.Close("c"),
                MiniXML.Open("c"),
                MiniXML.Close("c"),
                MiniXML.Open("d"),
                MiniXML.Close("d"),
                MiniXML.Open("d"),
                MiniXML.Close("d"),
                MiniXML.Open("a"),
                  MiniXML.Open("b"),
                    MiniXML.Open("c"),
                      MiniXML.Open("c"),
                      MiniXML.Close("c"),
                    MiniXML.Close("c"),
                    MiniXML.Open("d"),
                    MiniXML.Close("d"),
                    MiniXML.Open("d"),
                    MiniXML.Close("d"),
                  MiniXML.Close("b"),
                  MiniXML.Open("b"),
                    MiniXML.Open("d"),
                    MiniXML.Close("d"),
                  MiniXML.Close("b"),
                MiniXML.Close("a"),
                MiniXML.Open("b"),
                  MiniXML.Open("d"),
                  MiniXML.Close("d"),
                MiniXML.Close("b"),
                MiniXML.Open("d"),
                MiniXML.Close("d"),
                // format: on
              ),
              events
            ))
      }
  }

  test("let-for") {
    val mft =
      MiniXQueryCompiler
        .compile(
          Query.LetClause(
            "a",
            Query.Ordpath(MiniXPath(NonEmptyList.one(Step.Descendant(Some("a")))), None),
            Query.ForClause(
              "b",
              MiniXPath(NonEmptyList.one(Step.Descendant(Some("b")))),
              Query.Node("res", Query.Sequence(NonEmptyList.of(Query.Variable("a"), Query.Variable("b"))))
            )
          ),
          credit
        )

    mft
      .esp[IO]
      .flatMap { esp =>
        Stream
          .emits(
            List[MiniXML](
              // format: off
              MiniXML.Open("doc"),
                MiniXML.Open("b"),
                  MiniXML.Text("some b"),
                MiniXML.Close("b"),
                MiniXML.Open("b"),
                  MiniXML.Text("another b"),
                MiniXML.Close("b"),
              MiniXML.Close("doc"),
                MiniXML.Open("a"),
                  MiniXML.Text("some a"),
                MiniXML.Close("a"),
              // format: on
            )
          )
          .through(esp.pipe)
          .compile
          .toList
          .map(events =>
            expect.same(
              List[MiniXML](
                // format: off
                MiniXML.Open("res"),
                  MiniXML.Open("a"),
                    MiniXML.Text("some a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                    MiniXML.Text("some b"),
                  MiniXML.Close("b"),
                MiniXML.Close("res"),
                MiniXML.Open("res"),
                  MiniXML.Open("a"),
                    MiniXML.Text("some a"),
                  MiniXML.Close("a"),
                  MiniXML.Open("b"),
                    MiniXML.Text("another b"),
                  MiniXML.Close("b"),
                MiniXML.Close("res"),
                // format: on
              ),
              events
            ))
      }
  }

}
