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

package fs2.data
package json
package jq

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.{Eq, Show}

import pfsa._
import tagged._
import Pred.syntax._

private sealed trait TaggedMatcher {
  def dnf: NonEmptyList[NonEmptyList[AtomTaggedMatcher]] =
    this match {
      case TaggedMatcher.AllOf(clauses) =>
        clauses.toList match {
          case Nil      => NonEmptyList.one(NonEmptyList.one(TaggedMatcher.Fail))
          case c :: Nil => c.dnf
          case c :: rest =>
            val dnf1 = c.dnf
            val dnf2 = TaggedMatcher.AllOf(rest.toSet).dnf
            dnf1.flatMap(conj1 => dnf2.map(conj2 => conj1.concatNel(conj2)))
        }
      case TaggedMatcher.AnyOf(clauses) =>
        NonEmptyList
          .fromList(clauses.toList)
          .getOrElse(NonEmptyList.one(TaggedMatcher.Fail))
          .flatMap(_.dnf)
      case atom: AtomTaggedMatcher =>
        NonEmptyList.one(NonEmptyList.one(atom))
    }
}
private sealed trait AtomTaggedMatcher extends TaggedMatcher
private sealed trait PatternTaggedMatcher extends AtomTaggedMatcher
private sealed trait GuardTaggedMatcher extends AtomTaggedMatcher
private sealed trait NegatableTaggedMatcher extends AtomTaggedMatcher
private object TaggedMatcher {

  case object StartObject extends PatternTaggedMatcher with NegatableTaggedMatcher
  case object StartArray extends PatternTaggedMatcher with NegatableTaggedMatcher

  case class Field(name: String) extends PatternTaggedMatcher with NegatableTaggedMatcher
  case class Index(idx: Int) extends PatternTaggedMatcher with NegatableTaggedMatcher
  case class Slice(start: Int, end: Option[Int]) extends GuardTaggedMatcher with NegatableTaggedMatcher
  case object Any extends PatternTaggedMatcher
  case object Fail extends AtomTaggedMatcher

  case class AnyOf(m: Set[TaggedMatcher]) extends TaggedMatcher
  case class AllOf(m: Set[TaggedMatcher]) extends TaggedMatcher
  case class Not(p: NegatableTaggedMatcher) extends GuardTaggedMatcher

  implicit object TaggedInstances extends Pred[TaggedMatcher, TaggedJson] with Candidate[TaggedMatcher, TaggedJson] {

    override def pick(set: TaggedMatcher): Option[TaggedJson] =
      set match {
        case StartObject          => Some(TaggedJson.Raw(Token.StartObject))
        case StartArray           => Some(TaggedJson.Raw(Token.StartArray))
        case Field(name)          => Some(TaggedJson.StartObjectValue(name))
        case Index(idx)           => Some(TaggedJson.StartArrayElement(idx))
        case Slice(start, _)      => Some(TaggedJson.StartArrayElement(start))
        case Any                  => Some(TaggedJson.Raw(Token.StartObject))
        case Fail                 => None
        case AnyOf(m)             => m.headOption.flatMap(pick(_))
        case AllOf(m)             => m.headOption.flatMap(pick(_))
        case Not(StartObject)     => Some(TaggedJson.Raw(Token.StartArray))
        case Not(StartArray)      => Some(TaggedJson.Raw(Token.StartObject))
        case Not(Field(name))     => Some(TaggedJson.StartObjectValue(s"!$name"))
        case Not(Index(idx))      => Some(TaggedJson.StartArrayElement(idx + 1))
        case Not(Slice(start, _)) => Some(TaggedJson.StartArrayElement(start - 1))
      }

    override def satsifies(p: TaggedMatcher)(e: TaggedJson): Boolean =
      (p, e) match {
        case (StartObject, TaggedJson.Raw(Token.StartObject))       => true
        case (StartArray, TaggedJson.Raw(Token.StartArray))         => true
        case (Field(name1), TaggedJson.StartObjectValue(name2))     => name1 === name2
        case (Index(idx1), TaggedJson.StartArrayElement(idx2))      => idx1 === idx2
        case (Slice(start, end), TaggedJson.StartArrayElement(idx)) => idx >= start && end.forall(idx < _)
        case (Any, _)                                               => true
        case (Fail, _)                                              => false
        case (AnyOf(ps), _)                                         => ps.exists(satsifies(_)(e))
        case (AllOf(ps), _)                                         => ps.forall(satsifies(_)(e))
        case (Not(p), _)                                            => !satsifies(p)(e)
        case (_, _)                                                 => false
      }

    override def always: TaggedMatcher = Any

    override def never: TaggedMatcher = Fail

    override def and(p1: TaggedMatcher, p2: TaggedMatcher): TaggedMatcher =
      (p1, p2) match {
        case (StartObject, StartObject)         => StartObject
        case (StartObject, _)                   => Fail
        case (_, StartObject)                   => Fail
        case (StartArray, StartArray)           => StartArray
        case (StartArray, _)                    => Fail
        case (_, StartArray)                    => Fail
        case (Any, _)                           => p2
        case (_, Any)                           => p1
        case (Fail, _)                          => Fail
        case (_, Fail)                          => Fail
        case (AllOf(ps1), AllOf(ps2))           => AllOf(ps1 ++ ps2)
        case (AllOf(ps1), _)                    => AllOf(ps1 + p2)
        case (_, AllOf(ps2))                    => AllOf(ps2 + p1)
        case (Field(f1), Field(f2))             => if (f1 === f2) p1 else Fail
        case (Field(_), Index(_) | Slice(_, _)) => Fail
        case (Index(_) | Slice(_, _), Field(_)) => Fail
        case (Index(idx1), Index(idx2))         => if (idx1 === idx2) p1 else Fail
        case (Index(idx), Slice(start, end))    => if (idx >= start && end.forall(idx < _)) p1 else Fail
        case (Slice(start, end), Index(idx))    => if (idx >= start && end.forall(idx < _)) p1 else Fail
        case (_, _)                             => if (p1 === p2) p1 else AllOf(Set(p1, p2))
      }

    override def or(p1: TaggedMatcher, p2: TaggedMatcher): TaggedMatcher =
      (p1, p2) match {
        case (AnyOf(ps1), AnyOf(ps2))                                               => AnyOf(ps1 ++ ps2)
        case (Any, _)                                                               => Any
        case (_, Any)                                                               => Any
        case (Fail, _)                                                              => p2
        case (_, Fail)                                                              => p1
        case (AnyOf(ps1), _)                                                        => AnyOf(ps1 + p2)
        case (_, AnyOf(ps2))                                                        => AnyOf(ps2 + p1)
        case (Index(idx), Slice(start, end)) if idx >= start && end.forall(idx < _) => p2
        case (Slice(start, end), Index(idx)) if idx >= start && end.forall(idx < _) => p1
        case (_, _) => if (p1 === p2) p1 else AnyOf(Set(p1, p2))
      }

    override def not(p: TaggedMatcher): TaggedMatcher =
      p match {
        case Not(p)                    => p
        case Fail                      => Any
        case Any                       => Fail
        case AllOf(ps)                 => ps.fold(never)((acc, p) => or(acc, not(p)))
        case AnyOf(ps)                 => ps.fold(always)((acc, p) => and(acc, not(p)))
        case p: NegatableTaggedMatcher => Not(p)
      }

    override def isSatisfiable(p: TaggedMatcher): Boolean =
      p match {
        case Fail         => false
        case AnyOf(cases) => cases.exists(_.isSatisfiable)
        case AllOf(cases) => cases.forall(_.isSatisfiable)
        case _            => true
      }

  }

  implicit val eq: Eq[TaggedMatcher] = Eq.fromUniversalEquals

}

private object NegatableTaggedMatcher {

  implicit val show: Show[NegatableTaggedMatcher] = {
    case TaggedMatcher.StartObject             => ". != {"
    case TaggedMatcher.StartArray              => ". != ["
    case TaggedMatcher.Field(name)             => show". != {$name}"
    case TaggedMatcher.Index(idx)              => show". != [$idx]"
    case TaggedMatcher.Slice(start, Some(end)) => show". not in [$start..$end]"
    case TaggedMatcher.Slice(start, None)      => show". != [$start]"
  }

}

private object GuardTaggedMatcher {
  implicit val show: Show[GuardTaggedMatcher] = {
    case TaggedMatcher.Not(p)                  => show"$p"
    case TaggedMatcher.Slice(start, Some(end)) => show". in [$start..$end]"
    case TaggedMatcher.Slice(start, None)      => show". == [$start]"
  }
}
