/*
 * Copyright 2021 Lucas Satabin
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
package kleenex

import transducer.CharRanges

import cats.ApplicativeError
import cats.data.NonEmptyList
import cats.parse.{Caret, LocationMap, Parser0, Parser => P}
import cats.syntax.all._

case class KleenexParseException(msg: String) extends Exception(msg)

class KleenexParser[F[_]](implicit F: ApplicativeError[F, Throwable]) {

  def parse(content: String): F[Program] =
    KleenexParser.program
      .parseAll(content)
      .leftMap { e =>
        val locations = LocationMap(content)
        KleenexParseException(prettyprint(locations, e))
      }
      .liftTo[F]

  private def description(x: P.Expectation): String = x match {
    case P.Expectation.OneOfStr(_, List(str)) =>
      s"expected $str"
    case P.Expectation.OneOfStr(_, strs) =>
      val strList = strs.map(x => s"'$x'").mkString(", ")
      s"expected one of $strList"
    case P.Expectation.InRange(_, lower, upper) =>
      if (lower == upper) s"expected '$lower'"
      else s"expected '$lower' ~ '$upper'"
    case P.Expectation.StartOfString(_) =>
      "expected beginning of file"
    case P.Expectation.EndOfString(_, _) =>
      "expected end of file"
    case P.Expectation.Length(_, expected, actual) =>
      s"unexpected eof; expected ${expected - actual} more characters"
    case P.Expectation.ExpectedFailureAt(_, matched) =>
      s"unexpected '$matched'"
    case P.Expectation.Fail(_) =>
      "failed to parse"
    case P.Expectation.FailWith(_, message) =>
      message
    case P.Expectation.WithContext(contextStr, _) =>
      s"expected $contextStr"
  }

  private def prettyprint(locmap: LocationMap, x: P.Expectation): String = {
    val (row, col) = locmap.toLineCol(x.offset).getOrElse((0, locmap.input.size))
    val (r, c) = (row + 1, col + 1)
    val line = locmap.getLine(row).get
    val offending =
      s"${row.toString map { _ => ' ' }} | ${" " * col}^"
    s"""
      |$r:$c: error: ${description(x)}
      |$r | $line
      |$offending""".stripMargin
  }

  private def prettyprint(locmap: LocationMap, x: P.Error): String =
    x.expected.map(prettyprint(locmap, _)).toList.mkString("")
}

object KleenexParser {
  import P._

  private[this] val whitespace: P[Unit] = oneOf(List(charIn(" \t\r\n"), string("//") ~ charsWhile(_ != '\n'))).void
  private[this] val whitespaces0: Parser0[Unit] = whitespace.rep0.void

  private val regIdentStart = ('a' to 'z')
  private val identStart = regIdentStart ++ ('A' to 'Z')
  private val digit = ('0' to '9')
  private val identChar = identStart ++ digit ++ List('-', '_')
  private val hexDigit = digit ++ ('a' to 'f') ++ ('A' to 'F')

  private val ident: P[String] =
    (peek(charIn(identStart)).with1 *> charsWhile(identChar.contains(_)))
      .withContext("identifier") <* whitespaces0

  private val regIdent: P[String] =
    (peek(charIn(regIdentStart)).with1 *> charsWhile(identChar.contains(_)))
      .withContext("register identifier (must start with lower case)") <* whitespaces0

  private val str: P[String] =
    oneOf(
      List(
        charsWhile(!"\\\"".contains(_)).string,
        char('\\') *> oneOf(List(
          char('"').as("\""),
          char('\\').as("\\"),
          char('r').as("\r"),
          char('n').as("\n"),
          char('t').as("\t"),
          char('f').as("\f"),
          char('x') *> charIn(hexDigit)
            .rep(min = 2, max = 2)
            .string
            .map(codepoint => Character.toString(Integer.parseInt(codepoint, 16))),
          char('u') *> charIn(hexDigit)
            .rep(min = 4, max = 4)
            .string
            .map(codepoint => Character.toString(Integer.parseInt(codepoint, 16)))
        ))
      )
    ).rep0.map(_.combineAll).with1.surroundedBy(char('"')) <* whitespaces0

  private val integer: P[Int] =
    charIn(digit).rep.string.mapFilter(_.toIntOption).withContext("positive integer")

  def keyword(kw: String): P[Unit] =
    string(kw) <* whitespaces0

  private val range: P[(Int, Option[Int])] =
    char('{') *> oneOf(
      List(
        char(',') *> integer.map(max => (0, Some(max))),
        (integer ~ (char(',') *> integer.?).?).map {
          case (min, None)            => (min, Some(min))
          case (min, Some(None))      => (min, None)
          case (min, Some(Some(max))) => (min, Some(max))
        }
      )) <* char('}')

  val regex: P[Regex] = P.recursive[Regex] { regex =>
    val setChar = oneOf(
      List(
        charWhere(!"-]\\".contains(_)),
        char('\\') *> oneOf(List(
          char('\\').as('\\'),
          char('/').as('/'),
          char('-').as('-'),
          char(']').as(']'),
          char('[').as('['),
          char('r').as('\r'),
          char('n').as('\n'),
          char('t').as('\t'),
          char('f').as('\f')
        ))
      ))
    val set = char('[') *> (char('^').as(false).?.map(_.getOrElse(true)) ~ oneOf(List(
      char('-').as(('-', '-') :: Nil),
      (setChar ~ (char('-') *> setChar.?).?).map {
        case (fst, Some(Some(snd))) => (fst, snd) :: Nil
        case (fst, Some(None))      => (fst, fst) :: ('-', '-') :: Nil
        case (fst, None)            => (fst, fst) :: Nil
      }
    )).rep0.map(_.flatten)).map {
      case (false, Nil)            => CharRanges.all
      case (true, Nil)             => CharRanges.empty
      case (true, r :: Nil)        => CharRanges.range(r)
      case (false, r :: Nil)       => CharRanges.range(r).invert
      case (true, r1 :: r2 :: rs)  => CharRanges.ranges(r1, r2, rs: _*)
      case (false, r1 :: r2 :: rs) => CharRanges.ranges(r1, r2, rs: _*).invert
    } <* char(']')

    val atom =
      oneOf(
        List(
          char('.').as(Regex.Any),
          set.map(Regex.Set(_)),
          oneOf(
            List(
              charWhere(!"\\/?*+|{[().".contains(_)).string,
              char('\\') *> oneOf(List(
                char('/').as("/"),
                char('\\').as("\\"),
                char('r').as("\r"),
                char('n').as("\n"),
                char('t').as("\t"),
                char('f').as("\f"),
                char('?').as("?"),
                char('*').as("*"),
                char('+').as("+"),
                char('|').as("|"),
                char('{').as("{"),
                char('[').as("["),
                char('(').as("("),
                char(')').as(")"),
                char('.').as("."),
                char('u') *> charIn(hexDigit)
                  .rep(min = 4, max = 4)
                  .string
                  .map(codepoint => Character.toString(Integer.parseInt(codepoint, 16)))
              ))
            )
          ).map(Regex.Str(_)),
          regex.between(char('('), char(')'))
        ))

    val greedy = char('?').?.map(_.isEmpty)
    val suffixed =
      (atom ~ oneOf(
        List(
          char('?') *> greedy.map(greedy => Regex.Question(_, greedy)),
          char('+') *> greedy.map(greedy => Regex.Plus(_, greedy)),
          char('*') *> greedy.map(greedy => Regex.Star(_, greedy)),
          range.map { case (min, max) => Regex.Range(_, min, max) }
        )).?)
        .map {
          case (atom, None)      => atom
          case (atom, Some(mod)) => mod(atom)
        }

    def aggregateStr(seq: NonEmptyList[Regex]): NonEmptyList[Regex] = {
      def loop(seq: NonEmptyList[Regex]): NonEmptyList[Regex] =
        seq match {
          case NonEmptyList(Regex.Str(s1), Regex.Str(s2) :: rest) => loop(NonEmptyList(Regex.Str(s1 + s2), rest))
          case NonEmptyList(re, r :: rest)                        => re :: loop(NonEmptyList(r, rest))
          case NonEmptyList(_, Nil)                               => seq
        }
      loop(seq)
    }

    val seq =
      suffixed.rep
        .map(aggregateStr(_))
        .map {
          case NonEmptyList(atom, Nil) => atom
          case seq                     => Regex.Concat(seq)
        }

    seq.repSep(keyword("|")).map {
      case NonEmptyList(seq, Nil) => seq
      case alts                   => Regex.Or(alts)
    }
  }

  private val registerUpdate: P[Term] =
    (caret.with1 ~ regIdent ~ oneOf(List(keyword("<-").as(false), keyword("+=").as(true))) ~ oneOf(
      List(str.map(RegOrStr.Str(_)), regIdent.map(RegOrStr.Reg(_)))).rep)
      .map { case (((caret, reg), prepend), value) =>
        Term.UpdateReg(reg, if (prepend) RegOrStr.Reg(reg) :: value else value)(caret)
      }

  val term: P[Term] = recursive[Term] { term =>
    val atom: P[Term] =
      oneOf(
        List(
          caret.map(Term.One()).with1 <* keyword("1"),
          (caret.with1 ~ str).map { case (pos, s) => Term.Str(s)(pos) },
          ((caret.with1 ~ ident).map { case (pos, v) => Term.Var(v)(pos) } <* !oneOf(
            List(keyword(":="), keyword("@")))).backtrack,
          (caret.with1 ~ (char('/') *> regex <* char('/'))).map { case (pos, re) => Term.RE(re)(pos) } <* whitespaces0,
          (caret.with1 ~ (char('!') *> regIdent)).map { case (pos, reg) => Term.Output(reg)(pos) },
          registerUpdate.between(keyword("["), keyword("]")),
          term.between(keyword("("), keyword(")"))
        ))

    val suffixed: P[Term] =
      (atom ~ oneOf[Term => Term](List(
        (caret.with1 <* keyword("*")).map(pos => Term.Star(_)(pos)),
        (caret.with1 <* keyword("+")).map(pos => Term.Plus(_)(pos)),
        (caret.with1 <* keyword("?")).map(pos => Term.Question(_)(pos)),
        (caret.with1 ~ range).map { case (pos, (min, max)) => Term.Range(_: Term, min, max)(pos) } <* whitespaces0
      )).?)
        .map {
          case (inner, None)      => inner
          case (inner, Some(mod)) => mod(inner)
        }

    val prefixed: P[Term] =
      oneOf(
        List(
          (caret.with1 ~ (keyword("~") *> suffixed)).map { case (pos, t) => Term.Suppress(t)(pos) },
          (caret.with1 ~ (regIdent <* keyword("@")).backtrack ~ suffixed).map { case ((pos, reg), t) =>
            Term.Capture(reg, t)(pos)
          },
          suffixed
        ))

    val seq: P[Term] =
      prefixed.rep.map {
        case NonEmptyList(atom, Nil) => atom
        case seq                     => Term.Concat(seq)
      }

    seq.repSep(keyword("|")).map {
      case NonEmptyList(seq, Nil) => seq
      case alts                   => Term.Alternative(alts)
    }
  }

  val production: P[Production] =
    (caret.with1 ~ (ident <* keyword(":=")) ~ term).map { case ((pos, id), t) => Production(id, t)(pos) }

  private val pipeline: Parser0[(Caret, NonEmptyList[String])] =
    caret ~ oneOf0(List(keyword("start:") *> ident.repSep(keyword(">>")), pure(NonEmptyList.one("main"))))

  val program: P[Program] =
    (pipeline.with1 ~ production.rep)
      .map { case ((pos, pipe), rules) => Program(pipe, rules)(pos) }
      .surroundedBy(whitespaces0)

}
