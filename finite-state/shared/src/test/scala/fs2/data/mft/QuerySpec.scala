package fs2
package data
package mft
package query

import cats.Eq
import cats.effect.{IO, Resource}
import fs2.data.esp.{Conversion, ESP, Tag}
import fs2.data.pattern.{ConstructorTree, Evaluator}
import weaver._

import pfsa.{Candidate, Pred, Regular}
import cats.data.NonEmptyList

object QuerySpec extends IOSuite {

  type Res = ESP[IO, NonEmptyList[Set[String]], String, String]

  implicit object StringConversions extends Conversion[String, MiniXML] {

    override def makeOpen(t: String): MiniXML = MiniXML.Open(t)

    override def makeClose(t: String): MiniXML = MiniXML.Close(t)

    override def makeLeaf(t: String): MiniXML = MiniXML.Text(t)

  }

  implicit object evaluator extends Evaluator[NonEmptyList[Set[String]], Tag[String]] {

    override def eval(guard: NonEmptyList[Set[String]], tree: ConstructorTree[Tag[String]]): Option[Tag[String]] =
      tree match {
        case ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(n), _))) if guard.forall(_.contains(n)) =>
          Some(Tag.True)
        case ConstructorTree(Tag.Name(n), _) if guard.forall(_.contains(n)) => Some(Tag.True)
        case _                                                              => None
      }

  }

  object MiniXQueryCompiler extends QueryCompiler[String, MiniXPath] {

    type Matcher = Set[String]
    type Char = String
    type Pattern = Option[String]
    type Guard = Set[String]

    override implicit object predicate extends Pred[Matcher, Char] {

      override def satsifies(p: Matcher)(e: Char): Boolean = p.contains(e)

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
        case (acc, Step.Child(name)) =>
          acc ~ Regular.chars(Set(name))
        case (acc, Step.Descendant(name)) =>
          acc ~ Regular.any.rep ~ Regular.chars(Set(name))
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

  override def sharedResource: Resource[IO, Res] = Resource.eval {
    val mft =
      MiniXQueryCompiler.compile(
        Query.ForClause(
          "v1",
          MiniXPath(NonEmptyList.one(Step.Descendant("a"))),
          Query.ForClause(
            "v2",
            MiniXPath(NonEmptyList.one(Step.Descendant("b"))),
            Query.LetClause(
              "v3",
              Query.Ordpath(MiniXPath(NonEmptyList.one(Step.Descendant("c")))),
              Query.LetClause(
                "v4",
                Query.Ordpath(MiniXPath(NonEmptyList.one(Step.Descendant("d")))),
                Query.Sequence(NonEmptyList
                  .of(Query.Variable("v1"), Query.Variable("v2"), Query.Variable("v3"), Query.Variable("v4")))
              )
            )
          )
        ))
    mft.esp[IO]
  }

  test("simple query") { esp =>
    Stream
      .emits(
        List[MiniXML](
          // format: off
          MiniXML.Open("doc"),
            MiniXML.Open("a"),
              MiniXML.Open("b"),
                MiniXML.Open("c"),
                MiniXML.Close("c"),
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
      .covary[IO]
      .debug(s => s"before: $s")
      .through(esp.pipe)
      .debug(s => s"after: $s")
      .compile
      .drain
      .as(expect(true))
  }

}
