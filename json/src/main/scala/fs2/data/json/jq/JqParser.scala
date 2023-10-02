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

package fs2.data.json.jq

import cats.data.NonEmptyList
import cats.parse.{Accumulator0, Appender, Numbers, Parser => P, Parser0}
import cats.syntax.all._
import cats.{MonadThrow, Monoid}

case class JqParserException(error: P.Error) extends Exception(error.show)

object JqParser {

  implicit def accForMonoid[T: Monoid]: Accumulator0[T, T] = new Accumulator0[T, T] {
    override def newAppender(): Appender[T, T] = new Appender[T, T] {
      private[this] var res = Monoid[T].empty
      override def append(item: T) = {
        res = res.combine(item)
        this
      }
      override def finish(): T = res
    }

  }

  private val whitespace: P[Char] = P.charIn(" \t\r\n")
  private val whitespace0: Parser0[Unit] = whitespace.rep0.void

  private val identifierChar: P[Unit] =
    P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "-_").void
  private val identifier: P[String] =
    (P.charIn(('a' to 'z') ++ ('A' to 'Z')) ~ identifierChar.rep0)
      .withContext("identifier")
      .string <* whitespace0

  private def kw(kw: String): P[Unit] =
    (P.string(kw) ~ !identifierChar).void <* whitespace0

  private val kwTrue: P[Unit] = kw("true")
  private val kwFalse: P[Unit] = kw("false")
  private val kwNull: P[Unit] = kw("null")

  private def ch(c: Char): P[Unit] =
    P.char(c) <* whitespace0

  private def str(s: String): P[Unit] =
    P.string(s) <* whitespace0

  private val string: P[String] =
    P.char('"') *> P
      .oneOf(
        P.charsWhile(c => c != '"' && c != '\\') ::
          (P.char('\\') *> P.fromCharMap(Map('"' -> "\"", '\\' -> "\\"))) ::
          Nil)
      .repAs0[String] <* ch('"')

  private val index: P[Int] =
    Numbers.nonNegativeIntString.mapFilter(s => Either.catchNonFatal(s.toInt).toOption) <* whitespace0

  private val filter: P[Filter] = {
    val access: P[Filter] =
      P.oneOf(
        string.map(Jq.Field(_)) ::
          (index ~ (ch(':') *> index.?).?)
            .collect {
              case (idx, None) =>
                Jq.Index(idx)
              case (idx1, Some(Some(idx2))) if idx1 == idx2 =>
                Jq.Index(idx1)
              case (min, Some(max)) if max.forall(min < _) =>
                Jq.Slice(min, max)
            } ::
          (ch(':') *> index.map(max => Jq.Slice(0, Some(max)))) ::
          Nil)
        .between(ch('['), ch(']'))
        .withContext("string, index, slice 'min:max' (with min <= max), slice 'idx:', or slice ':idx'")

    val step: P[Filter] =
      (ch('.') *> P
        .oneOf(
          identifier.map(Jq.Field(_)) ::
            access.backtrack ::
            Nil)
        .? ~ access.backtrack.repAs0[Filter])
        .map {
          case (Some(fst), snd) => fst ~ snd
          case (None, access)   => Jq.Identity ~ access
        }
        .repAs[Filter]

    P.oneOf(
      (str("..") *> (access.backtrack ~ step.repAs0[Filter]).?).map {
        case Some((access, rest)) => Jq.RecursiveDescent ~ access ~ rest
        case None                 => Jq.RecursiveDescent
      } ::
        step ::
        Nil)
      // repSepAs would be great here
      .repSep(ch('|'))
      .map {
        case NonEmptyList(filter, Nil) => filter
        case steps                     => steps.reduceLeft(_ ~ _)
      }
  }

  private val selector: P[(Filter, Jq => Jq)] = P.recursive[(Filter, Jq => Jq)] { selector =>
    (filter ~ (str("[]") *> selector.?).?).map {
      case (prefix, None) =>
        (prefix, identity)
      case (prefix, Some(None)) =>
        (Jq.Identity, Jq.Iterator(prefix, _))
      case (prefix1, Some(Some((prefix2, f)))) =>
        (prefix2, inner => Jq.Iterator(prefix1, f(inner)))
    }
  }

  private val query: P[Jq] = P.recursive[Jq] { query =>
    val constructor: P[Filter => Constructor] =
      P.oneOf(
        query
          .repSep0(ch(','))
          .with1
          .between(ch('['), ch(']'))
          .map[Filter => Constructor](fs => prefix => Jq.Arr(prefix, fs)) ::
          (string ~ (ch(':') *> query))
            .repSep0(ch(','))
            .with1
            .between(ch('{'), ch('}'))
            .map[Filter => Constructor](fs => prefix => Jq.Obj(prefix, fs)) ::
          string.map[Filter => Constructor](s => _ => Jq.Str(s)) ::
          kwTrue.as[Filter => Constructor](_ => Jq.Bool(true)) ::
          kwFalse.as[Filter => Constructor](_ => Jq.Bool(false)) ::
          kwNull.as[Filter => Constructor](_ => Jq.Null) ::
          Numbers.jsonNumber.map[Filter => Constructor](n => _ => Jq.Num(n)) ::
          Nil)

    whitespace0.with1 *>
      P.oneOf(
        (selector ~ (ch('|') *> constructor).?).map {
          case ((filter, f), Some(cst)) =>
            f(cst(filter))
          case ((filter, f), None) => f(filter)
        } ::
          constructor.map(cst => cst(Jq.Identity)) ::
          Nil)
  }

  def parse[F[_]](input: String)(implicit F: MonadThrow[F]): F[Jq] =
    either(input).liftTo[F]

  def either(input: String): Either[Throwable, Jq] =
    (query <* P.end).parseAll(input).leftMap(JqParserException(_))

}
