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
package jq
package internal

import cats.data.{NonEmptyList, StateT}
import cats.syntax.all._
import cats.{Defer, Eq, MonadThrow}

import mft.query.{Query, QueryCompiler}
import tagged.TaggedJson
import pfsa._

private[jq] class ESPJqCompiler[F[_]](implicit F: MonadThrow[F], defer: Defer[F])
    extends QueryCompiler[TaggedJson, TaggedJson, Filter]
    with Compiler[F] {

  override protected val emitSelected: Boolean = false

  private type State[T] = StateT[F, Int, T]

  private def nextIdent: State[String] =
    for {
      id <- StateT.get
      _ <- StateT.set(id + 1)
    } yield s"v$id"

  private def pure[T](v: T): State[T] =
    StateT.pure(v)

  private def raiseError[T](exn: Throwable): State[T] =
    exn.raiseError[State, T]

  type Matcher = TaggedMatcher
  type Guard = GuardTaggedMatcher
  type Pattern = PatternTaggedMatcher
  type Char = TaggedJson

  override implicit def predicate: Pred[Matcher, Char] = TaggedMatcher.TaggedInstances

  override implicit def candidate: Candidate[Matcher, Char] = TaggedMatcher.TaggedInstances

  override implicit def charsEq: Eq[TaggedMatcher] = TaggedMatcher.eq

  override def tagOf(pattern: PatternTaggedMatcher): Option[TaggedJson] = pattern match {
    case TaggedMatcher.StartJson   => Some(TaggedJson.StartJson)
    case TaggedMatcher.StartObject => Some(TaggedJson.Raw(Token.StartObject))
    case TaggedMatcher.StartArray  => Some(TaggedJson.Raw(Token.StartArray))
    case TaggedMatcher.Field(name) => Some(TaggedJson.StartObjectValue(name))
    case TaggedMatcher.Index(idx)  => Some(TaggedJson.StartArrayElement(idx))
    case TaggedMatcher.Any         => None
  }

  def path2regular(f: Filter): Regular[TaggedMatcher] = {
    def loop(f: Filter): Regular[TaggedMatcher] =
      f match {
        case Jq.Root =>
          Regular.chars[TaggedMatcher](TaggedMatcher.StartJson)
        case Jq.Slice(start, end) =>
          Regular.chars[TaggedMatcher](TaggedMatcher.StartArray) ~ Regular.chars(TaggedMatcher.Slice(start, end))
        case Jq.Index(idx) =>
          Regular.chars[TaggedMatcher](TaggedMatcher.StartArray) ~ Regular.chars(TaggedMatcher.Index(idx))
        case Jq.Child =>
          (Regular.chars[TaggedMatcher](TaggedMatcher.StartArray) ||
            Regular.chars[TaggedMatcher](TaggedMatcher.StartObject)) ~ Regular.any
        case Jq.RecursiveDescent =>
          ((Regular.chars[TaggedMatcher](TaggedMatcher.StartArray) ||
            Regular.chars[TaggedMatcher](TaggedMatcher.StartObject)) ~ Regular.any).rep
        case Jq.Field(name) =>
          Regular.chars[TaggedMatcher](TaggedMatcher.StartObject) ~ Regular.chars(TaggedMatcher.Field(name))
        case Jq.Identity =>
          Regular.epsilon
        case Jq.Sequence(jqs) =>
          jqs.foldLeft(Regular.epsilon[TaggedMatcher])(_ ~ loop(_))
      }
    loop(f)
  }

  def cases(m: TaggedMatcher) = {
    // first transform the matcher into DNF
    m.dnf.toList
      // then for each conjunctive clause, separate the
      // pattern part (open tag or leaf) and the guard part
      // drop the statically false clauses
      .flatMap { atoms =>
        atoms.toList
          .foldLeftM[Option, (PatternTaggedMatcher, List[GuardTaggedMatcher])](
            (TaggedMatcher.Any, List.empty[GuardTaggedMatcher])) {
            case (_, TaggedMatcher.Fail) =>
              // fail the entire conjunction
              none
            case (acc, TaggedMatcher.Any) =>
              acc.some
            case ((TaggedMatcher.Any, guard), pattern: PatternTaggedMatcher) =>
              // this is a finer pattern, save it
              (pattern, guard).some
            case ((TaggedMatcher.Any, guard), g: GuardTaggedMatcher) =>
              // some guard for sure (almost)
              (TaggedMatcher.Any, g :: guard).some
            case ((pat1 @ TaggedMatcher.Field(fld1), guard), TaggedMatcher.Field(fld2)) =>
              // check whether both are compatible, and if yes, keep the most restrictive one
              (fld1, fld2) match {
                case (fld1, fld2) =>
                  if (fld1 === fld2) (pat1, guard).some
                  else none
              }
            case ((pat @ TaggedMatcher.Field(fld1), guard), TaggedMatcher.Not(TaggedMatcher.Field(fld2))) =>
              if (fld1 =!= fld2)
                // guard is redundant
                (pat, guard).some
              else
                // incompatible
                none
            case ((TaggedMatcher.Field(_), _),
                  TaggedMatcher.Index(_) | TaggedMatcher.Slice(_, _) | TaggedMatcher.StartObject |
                  TaggedMatcher.StartArray | TaggedMatcher.StartJson) =>
              // incompatible
              none
            case (acc @ (TaggedMatcher.Field(_), _),
                  TaggedMatcher.Not(
                    TaggedMatcher.Index(_) | TaggedMatcher.Slice(_, _) | TaggedMatcher.StartObject |
                    TaggedMatcher.StartArray | TaggedMatcher.StartJson)) =>
              // guard is redundant
              acc.some
            case ((pat1 @ TaggedMatcher.Index(idx1), guard), TaggedMatcher.Index(idx2)) =>
              // check whether both are compatible, and if yes, keep the most restrictive one
              (idx1, idx2) match {
                case (idx1, idx2) =>
                  if (idx1 === idx2) (pat1, guard).some
                  else none
              }
            case ((pat @ TaggedMatcher.Index(idx), guard), TaggedMatcher.Slice(start, end)) =>
              if (idx >= start && end.forall(idx <= _))
                // guard is redundant
                (pat, guard).some
              else
                // incompatible
                none
            case ((pat @ TaggedMatcher.Index(idx1), guard), TaggedMatcher.Not(TaggedMatcher.Index(idx2))) =>
              if (idx1 =!= idx2)
                // guard is redundant
                (pat, guard).some
              else // incompatible
                none
            case ((pat @ TaggedMatcher.Index(idx), guard), TaggedMatcher.Not(TaggedMatcher.Slice(start, end))) =>
              if (idx < start || end.exists(start > _))
                // guard is redundant
                (pat, guard).some
              else
                // incompatible
                none
            case (acc @ (TaggedMatcher.Index(_), _),
                  TaggedMatcher.Not(
                    TaggedMatcher.Field(_) | TaggedMatcher.StartObject | TaggedMatcher.StartArray |
                    TaggedMatcher.StartJson)) =>
              // redundant
              acc.some
            case ((TaggedMatcher.Index(_), _),
                  TaggedMatcher.Field(_) | TaggedMatcher.StartObject | TaggedMatcher.StartArray |
                  TaggedMatcher.StartJson) =>
              // incompatible
              none
            case (acc @ (TaggedMatcher.StartObject, _), TaggedMatcher.StartObject) =>
              // redundant
              acc.some
            case ((TaggedMatcher.StartObject, _), TaggedMatcher.Not(TaggedMatcher.StartObject)) =>
              // incompatible
              none
            case (acc @ (TaggedMatcher.StartObject, _), TaggedMatcher.Not(_)) =>
              // redundant
              acc.some
            case ((TaggedMatcher.StartObject, _), _) =>
              // incompatible
              none
            case (acc @ (TaggedMatcher.StartArray, _), TaggedMatcher.StartArray) =>
              // redundant
              acc.some
            case ((TaggedMatcher.StartArray, _), TaggedMatcher.Not(TaggedMatcher.StartArray)) =>
              // incompatible
              none
            case (acc @ (TaggedMatcher.StartArray, _), TaggedMatcher.Not(_)) =>
              // redundant
              acc.some
            case ((TaggedMatcher.StartArray, _), _) =>
              // incompatible
              none
            case (acc @ (TaggedMatcher.StartJson, _), TaggedMatcher.StartJson) =>
              // redundant
              acc.some
            case ((TaggedMatcher.StartJson, _), TaggedMatcher.Not(TaggedMatcher.StartJson)) =>
              // incompatible
              none
            case (acc @ (TaggedMatcher.StartJson, _), TaggedMatcher.Not(_)) =>
              // redundant
              acc.some
            case ((TaggedMatcher.StartJson, _), _) =>
              // incompatible
              none
          }
      }
  }

  private def preprocess(prefix: Filter, jq: Jq): State[Query[TaggedJson, Filter]] =
    jq match {
      case Jq.Null =>
        pure(Query.Leaf(TaggedJson.Raw(Token.NullValue)))
      case Jq.Bool(b) =>
        pure(Query.Leaf(TaggedJson.Raw(if (b) Token.TrueValue else Token.FalseValue)))
      case Jq.Arr(prefix1, values) =>
        values.zipWithIndex
          .traverse { case (elt, idx) =>
            preprocess(prefix ~ prefix1, elt).map(q => Query.Node(TaggedJson.StartArrayElement(idx), q))
          }
          .map { elts =>
            Query.Node(TaggedJson.Raw(Token.StartArray),
                       NonEmptyList.fromList(elts).fold(Query.empty[TaggedJson, Filter])(Query.Sequence(_)))
          }
      case Jq.Num(n) =>
        pure(Query.Leaf(TaggedJson.Raw(Token.NumberValue(n.toString()))))
      case Jq.Str(s) =>
        pure(Query.Leaf(TaggedJson.Raw(Token.StringValue(s))))
      case Jq.Obj(prefix1, fields) =>
        val (iterators, vs) =
          fields.zipWithIndex.partitionEither {
            case ((name, it @ Jq.Iterator(_, _)), idx) => Left((name, it, idx))
            case (kv, _)                               => Right(kv)
          }
        iterators match {
          case Nil =>
            vs.traverse { case (name, elt) =>
              preprocess(prefix ~ prefix1, elt).map(q => Query.Node(TaggedJson.StartObjectValue(name), q))
            }.map { elts =>
              Query.Node(TaggedJson.Raw(Token.StartObject),
                         NonEmptyList.fromList(elts).fold(Query.empty[TaggedJson, Filter])(Query.Sequence(_)))
            }
          case (name, Jq.Iterator(filter, inner), idx) :: Nil =>
            for {
              values <- vs.traverse { case (name, elt) =>
                for {
                  v <- nextIdent
                  q <- preprocess(prefix ~ prefix1, elt)
                } yield (v, Query.Node(TaggedJson.StartObjectValue(name), q))
              }
              v <- nextIdent
              inner <-
                if (inner == Jq.Identity)
                  Query.Variable[TaggedJson, Filter](v).pure[State]
                else
                  preprocess(Jq.Identity, inner)
            } yield {
              val (before, after) = values.splitAt(idx)
              val forClause: Query[TaggedJson, Filter] =
                Query.ForClause(
                  v,
                  prefix ~ prefix1 ~ filter ~ Jq.Child,
                  Query.Node(
                    TaggedJson.Raw(Token.StartObject),
                    Query.Sequence(
                      NonEmptyList[Query[TaggedJson, Filter]](Query.Node(TaggedJson.StartObjectValue(name), inner),
                                                              after.map(kv =>
                                                                Query.Variable[TaggedJson, Filter](kv._1)))
                        .prependList(before.map(kv => Query.Variable[TaggedJson, Filter](kv._1))))
                  )
                )
              values.foldLeft(forClause) { case (inner, (v, q)) => Query.LetClause(v, q, inner) }
            }
          case _ =>
            raiseError(
              JqException(s"object constructors may have only one iterator element, but got ${iterators.size}"))
        }
      case Jq.Iterator(filter, inner: Constructor) =>
        for {
          v <- nextIdent
          inner <- preprocess(Jq.Identity, inner)
        } yield Query.ForClause(v, prefix ~ filter ~ Jq.Child, inner)
      case Jq.Iterator(filter, inner) =>
        for {
          v <- nextIdent
          inner <- preprocess(Jq.Child, inner)
        } yield Query.ForClause(v, prefix ~ filter, inner)
      case filter: Filter =>
        pure(Query.Ordpath(prefix ~ filter))
    }

  def compile(jq: Jq): F[CompiledJq[F]] =
    for {
      query <- preprocess(Jq.Root, jq).runA(0)
      mft = compile(query)
      esp <- mft.esp
    } yield new ESPCompiledJq[F](esp)

}
