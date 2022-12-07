/*
 * Copyright 2022 Lucas Satabin
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

import cats.data.NonEmptyList
import cats.kernel.Eq
import cats.syntax.all._
import cats.{Defer, MonadError}
import fs2.data.mft.query.{Query, QueryCompiler}

import tagged.TaggedJson
import pfsa._

case class JqException(msg: String) extends Exception(msg)

private class Compiler[F[_]](implicit F: MonadError[F, Throwable], defer: Defer[F])
    extends QueryCompiler[JsonTag, TaggedJson, Filter] {

  type Matcher = TaggedMatcher
  type Guard = GuardTaggedMatcher
  type Pattern = PatternTaggedMatcher
  type Char = TaggedJson

  override implicit def predicate: Pred[Matcher, Char] = TaggedMatcher.TaggedInstances

  override implicit def candidate: Candidate[Matcher, Char] = TaggedMatcher.TaggedInstances

  override implicit def charsEq: Eq[TaggedMatcher] = TaggedMatcher.eq

  override def tagOf(pattern: PatternTaggedMatcher): Option[JsonTag] = ???

  def path2regular(f: Filter): Regular[TaggedMatcher] =
    f match {
      case Jq.Slice(start, end) =>
        Regular.chars[TaggedMatcher](TaggedMatcher.StartArray) ~ Regular.chars(TaggedMatcher.Slice(start, end))
      case Jq.Index(idx) =>
        Regular.chars[TaggedMatcher](TaggedMatcher.StartArray) ~ Regular.chars(TaggedMatcher.Index(idx))
      case Jq.RecursiveDescent =>
        ((Regular.chars[TaggedMatcher](TaggedMatcher.StartArray) ~ Regular.any) || (Regular
          .chars[TaggedMatcher](TaggedMatcher.StartObject) ~ Regular.any)).rep
      case Jq.Field(name) =>
        Regular.chars[TaggedMatcher](TaggedMatcher.StartObject) ~ Regular.chars(TaggedMatcher.Field(name))
      case Jq.Identity => Regular.epsilon
      case Jq.Sequence(jqs) =>
        jqs.foldLeft(Regular.epsilon[TaggedMatcher])(_ ~ path2regular(_))
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
            case ((pat @ TaggedMatcher.Any, guard), g: GuardTaggedMatcher) =>
              // some guard for sure (almost)
              (pat, g :: guard).some
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
                  TaggedMatcher.StartArray) =>
              // incompatbiel
              none
            case (acc @ (TaggedMatcher.Field(_), _),
                  TaggedMatcher.Not(
                    TaggedMatcher.Index(_) | TaggedMatcher.Slice(_, _) | TaggedMatcher.StartObject |
                    TaggedMatcher.StartArray)) =>
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
              if (idx >= start && end.forall(idx < _))
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
              if (idx >= start && end.forall(start < _))
                // guard is redundant
                (pat, guard).some
              else // incompatible
                none
            case (acc @ (TaggedMatcher.Index(_), _),
                  TaggedMatcher.Not(TaggedMatcher.Field(_) | TaggedMatcher.StartObject | TaggedMatcher.StartArray)) =>
              // redundant
              acc.some
            case ((TaggedMatcher.Index(_), _),
                  TaggedMatcher.Field(_) | TaggedMatcher.StartObject | TaggedMatcher.StartArray) =>
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
          }
      }
  }

  private def isForClause(q: Query[TaggedJson, Filter]): Boolean =
    q match {
      case Query.ForClause(_, _, _) => true
      case _                        => false
    }

  private def preprocess(jq: Jq): F[Query[TaggedJson, Filter]] =
    jq match {
      case Jq.Null =>
        F.pure(Query.Leaf(TaggedJson.Raw(Token.NullValue)))
      case Jq.Bool(b) =>
        F.pure(Query.Leaf(TaggedJson.Raw(if (b) Token.TrueValue else Token.FalseValue)))
      case Jq.Arr(values) =>
        values.traverse(preprocess(_)).flatMap { values =>
          val count = values.count(isForClause(_))
          if (count > 1) {
            F.raiseError(JqException(s"array constructors may have only one iterator elements, but got $count"))
          } else if (count == 0) {
            F.pure(
              Query.Node(
                TaggedJson.Raw(Token.StartArray),
                NonEmptyList
                  .fromList(values.mapWithIndex((elt, idx) => Query.Node(TaggedJson.StartArrayElement(idx), elt)))
                  .fold(Query.empty[TaggedJson, Filter])(Query.Sequence(_))
              ))
          } else {
            ???
          }
        }
      case Jq.Num(n) =>
        F.pure(Query.Leaf(TaggedJson.Raw(Token.NumberValue(n.toString()))))
      case Jq.Str(s) =>
        F.pure(Query.Leaf(TaggedJson.Raw(Token.StringValue(s))))
      case Jq.Obj(fields) =>
        ???
      case Jq.Iterator(filter, inner) =>
        ???
      case filter: Filter =>
        ???
    }

  def compile(jq: Jq): F[CompiledJq[F]] =
    for {
      query <- preprocess(jq)
      esp <- compile(query).esp
    } yield new CompiledJq[F](esp)

}
