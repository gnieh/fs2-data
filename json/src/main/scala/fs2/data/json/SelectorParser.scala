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

import cats._
import cats.implicits._

import scala.annotation.switch

case class JsonSelectorException(msg: String, idx: Int) extends Exception(msg)

/** Parses a filter string. Syntax is as follows:
  * {{{
  * Selector ::= `.`
  *            | Sel Sel*
  *
  * Sel ::= `.` Name `!`? `?`?
  *       | `.` `[` String (`,` String)* `]` `!`? `?`?
  *       | `.` `[` Integer (`,` Integer)* `]` `?`?
  *       | `.` `[` Integer `:` Integer `]` `?`?
  *       | `.` `[` `]` `?`?
  *
  * Name ::= [a-zA-Z_][a-zA-Z0-9_]*
  *
  * String ::= <a json string>
  *
  * Integer ::= 0
  *           | [1-9][0-9]*
  * }}}
  */
class SelectorParser[F[_]](val input: String)(implicit F: MonadError[F, Throwable]) {

  import SelectorParser._

  // === parser ===

  private def parseSeveralIndices(idx: Int, indices: Set[Int]): F[(Boolean => Selector, Int)] =
    next(idx).flatMap {
      case Some((Comma(_), idx)) =>
        next(idx).flatMap {
          case Some((Integer(i, _), idx)) => parseSeveralIndices(idx, indices + i)
          case Some((token, _)) =>
            F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
          case None =>
            F.raiseError(new JsonSelectorException("unexpected end of input", idx))
        }
      case Some((RightBracket(_), idx)) =>
        F.pure((Selector.IndexSelector(indices, _), idx))
      case Some((token, _)) =>
        F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
      case None =>
        F.raiseError(new JsonSelectorException("unexpected end of input", idx))
    }

  private def parseRangeIndices(idx: Int, start: Int): F[(Boolean => Selector, Int)] =
    next(idx).flatMap {
      case Some((Integer(end, _), idx)) =>
        next(idx).flatMap {
          case Some((RightBracket(_), idx)) => F.pure((Selector.IndexSelector(start, end, _), idx))
          case Some((token, _)) =>
            F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
          case None =>
            F.raiseError(new JsonSelectorException("unexpected end of input", idx))
        }
      case Some((token, _)) =>
        F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
      case None =>
        F.raiseError(new JsonSelectorException("unexpected end of input", idx))
    }

  private def parseArraySelector(idx: Int, fst: Int): F[(Boolean => Selector, Int)] =
    next(idx).flatMap {
      case Some((RightBracket(_), idx)) => F.pure((Selector.IndexSelector(fst, _), idx))
      case Some((Comma(_), _))          => parseSeveralIndices(idx, Set(fst))
      case Some((Colon(_), idx))        => parseRangeIndices(idx, fst)
      case Some((token, _)) =>
        F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
      case None =>
        F.raiseError(new JsonSelectorException("unexpected end of input", idx))
    }

  private def parseMandatory(idx: Int, names: Set[String]): F[(Boolean => Selector, Int)] =
    next(idx, peek = true).map {
      case Some((Bang(_), idx)) => (Selector.NameSelector(names, _, true), idx + 1)
      case _                    => (Selector.NameSelector(names, _, false), idx)
    }

  private def parseSeveralNames(idx: Int, names: Set[String]): F[(Boolean => Selector, Int)] =
    next(idx).flatMap {
      case Some((Comma(_), idx)) =>
        next(idx).flatMap {
          case Some((Str(s, _), idx)) => parseSeveralNames(idx, names + s)
          case Some((token, _)) =>
            F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
          case None =>
            F.raiseError(new JsonSelectorException("unexpected end of input", idx))
        }
      case Some((RightBracket(_), idx)) =>
        // check whether names are mandatory
        parseMandatory(idx, names)
      case Some((token, _)) =>
        F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
      case None =>
        F.raiseError(new JsonSelectorException("unexpected end of input", idx))
    }

  private def parseObjectSelector(idx: Int, fst: String): F[(Boolean => Selector, Int)] =
    next(idx).flatMap {
      case Some((RightBracket(_), idx)) => parseMandatory(idx, Set(fst))
      case Some((Comma(_), _))          => parseSeveralNames(idx, Set(fst))
      case Some((token, _)) =>
        F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
      case None =>
        F.raiseError(new JsonSelectorException("unexpected end of input", idx))
    }

  private def parseBracketed(idx: Int): F[(Boolean => Selector, Int)] =
    next(idx).flatMap {
      case Some((RightBracket(_), idx)) => F.pure((Selector.IteratorSelector(_), idx))
      case Some((Integer(fst, _), idx)) => parseArraySelector(idx, fst)
      case Some((Str(fst, _), idx))     => parseObjectSelector(idx, fst)
      case Some((token, _)) =>
        F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
      case None =>
        F.raiseError(new JsonSelectorException("unexpected end of input", idx))
    }

  private def parseSelector(idx: Int, fst: Boolean): F[(Selector, Int)] =
    next(idx).flatMap {
      case Some((Dot(_), idx)) =>
        val maker =
          next(idx).flatMap[(Boolean => Selector, Int)] {
            case Some((LeftBracket(_), idx)) =>
              parseBracketed(idx)
            case Some((Name(name, _), idx)) =>
              parseMandatory(idx, Set(name))
            case Some((token, _)) =>
              F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
            case None =>
              if (fst)
                F.pure((_ => Selector.ThisSelector, input.length))
              else
                F.raiseError(new JsonSelectorException("unexpected end of input", input.length))
          }
        maker.flatMap { case (maker, idx) =>
          next(idx).map {
            case Some((QuestionMark(_), idx)) => (maker(false), idx)
            case _                            => (maker(true), idx)
          }
        }
      case Some((token, _)) =>
        F.raiseError(new JsonSelectorException(s"invalid selector token '$token'", token.idx))
      case None =>
        F.raiseError(new JsonSelectorException("unexpected end of input", idx))
    }

  private def parseTail(idx: Int, left: Selector): F[(Selector, Int)] =
    next(idx).flatMap {
      case Some((_, _)) =>
        parseSelector(idx, false).flatMap { case (s, idx) =>
          parseTail(idx, s).map { case (tail, idx) =>
            (Selector.PipeSelector(left, tail), idx)
          }
        }
      case None =>
        F.pure((left, idx))
    }

  def parse(): F[Selector] =
    parseSelector(0, true)
      .flatMap { case (f, idx) =>
        parseTail(idx, f)
      }
      .map(_._1)

  // === lexer ===

  private def next(idx: Int, peek: Boolean = false): F[Option[(Token, Int)]] =
    if (idx >= input.length) {
      F.pure(None)
    } else {
      val c = input.charAt(idx)
      if (c.isWhitespace)
        next(idx + 1)
      else
        (c: @switch) match {
          case '[' => F.pure(Some((LeftBracket(idx), if (peek) idx else idx + 1)))
          case ']' => F.pure(Some((RightBracket(idx), if (peek) idx else idx + 1)))
          case '.' => F.pure(Some((Dot(idx), if (peek) idx else idx + 1)))
          case ':' => F.pure(Some((Colon(idx), if (peek) idx else idx + 1)))
          case ',' => F.pure(Some((Comma(idx), if (peek) idx else idx + 1)))
          case '?' => F.pure(Some((QuestionMark(idx), if (peek) idx else idx + 1)))
          case '!' => F.pure(Some((Bang(idx), if (peek) idx else idx + 1)))
          case '"' =>
            string(idx + 1).map { case (str, idx1) =>
              Some((Str(str, idx), if (peek) idx else idx1))
            }
          case _ =>
            if (c.isDigit) {
              integer(idx + 1, c).map { case (i, idx1) =>
                Some((Integer(i, idx), if (peek) idx else idx1))
              }
            } else if (c.isLetter || c == '_') {
              name(idx + 1, c).map { case (n, idx1) =>
                Some((Name(n, idx), if (peek) idx else idx1))
              }
            } else {
              F.raiseError(new JsonSelectorException(s"invalid selector character '$c'", idx))
            }
        }
    }

  // opening quote already read
  private def string(idx: Int): F[(String, Int)] = {
    def loop(idx: Int, acc: StringBuilder): F[(String, Int)] =
      if (idx >= input.length) {
        F.raiseError(new JsonSelectorException("unexpected end of input", idx))
      } else {
        val c = input.charAt(idx)
        if (c == '"') {
          F.pure((acc.result(), idx + 1))
        } else if (c == '\\') {
          if (idx + 1 >= input.size) {
            F.raiseError(new JsonSelectorException("unexpected end of input", idx + 1))
          } else {
            val c = input.charAt(idx + 1)
            (c: @switch) match {
              case '"'  => loop(idx + 2, acc.append('"'))
              case '\\' => loop(idx + 2, acc.append('\\'))
              case '/'  => loop(idx + 2, acc.append('/'))
              case 'b'  => loop(idx + 2, acc.append('\b'))
              case 'f'  => loop(idx + 2, acc.append('\f'))
              case 'n'  => loop(idx + 2, acc.append('\n'))
              case 'r'  => loop(idx + 2, acc.append('\r'))
              case 't'  => loop(idx + 2, acc.append('\t'))
              case 'u' =>
                val unicode =
                  (2 to 5).toList.foldLeftM(0) { case (n, i) =>
                    if (idx + i >= input.length) {
                      F.raiseError[Int](new JsonSelectorException("unexpected end of input", input.length))
                    } else {
                      val c = input.charAt(idx + i).toLower
                      if (c >= '0' && c <= '9')
                        F.pure((n << 4) | (0x0000000f & (c - '0')))
                      else if (c >= 'a' && c <= 'f')
                        F.pure((n << 4) | (0x0000000f & (c + 10 - 'a')))
                      else
                        F.raiseError[Int](new JsonSelectorException("malformed escaped unicode sequence", idx + i))
                    }
                  }
                unicode.flatMap(u => loop(idx + 6, acc.appendAll(Character.toChars(u))))
              case _ => F.raiseError(new JsonSelectorException(s"unknown escaped character '$c'", idx + 1))
            }
          }
        } else if (c >= 0x20 && c <= 0x10ffff) {
          loop(idx + 1, acc.append(c))
        } else {
          F.raiseError(new JsonSelectorException(s"invalid string character '$c'", idx))
        }
      }
    loop(idx, new StringBuilder)
  }

  private def name(idx: Int, fst: Char): F[(String, Int)] = {
    def loop(idx: Int, acc: StringBuilder): F[(String, Int)] =
      if (idx >= input.length) {
        F.pure((acc.result(), idx))
      } else {
        val c = input.charAt(idx)
        if (c == '_' || c.isLetter || c.isDigit)
          loop(idx + 1, acc.append(c))
        else
          F.pure((acc.result(), idx))
      }

    loop(idx, new StringBuilder().append(fst))
  }

  private def integer(idx: Int, fst: Char): F[(Int, Int)] = {
    def loop(idx: Int, acc: Int): F[(Int, Int)] =
      if (idx >= input.length) {
        F.pure((acc, idx))
      } else {
        val c = input.charAt(idx)
        if (c.isDigit)
          loop(idx + 1, acc * 10 + (c - '0'))
        else
          F.pure((acc, idx))
      }

    if (fst == '0')
      F.pure((0, idx))
    else
      loop(idx, fst - '0')
  }

}

object SelectorParser {
  def apply[F[_]](input: String)(implicit F: MonadError[F, Throwable]): F[Selector] =
    new SelectorParser[F](input).parse()
  // Mostly here for the literal selector macro, makes it simpler (esp with macro+kind-projector interaction)
  def either(input: String): Either[Throwable, Selector] = apply[Either[Throwable, *]](input)

  private sealed trait Token {
    val idx: Int
  }
  private case class LeftBracket(idx: Int) extends Token
  private case class RightBracket(idx: Int) extends Token
  private case class Dot(idx: Int) extends Token
  private case class Colon(idx: Int) extends Token
  private case class Comma(idx: Int) extends Token
  private case class QuestionMark(idx: Int) extends Token
  private case class Bang(idx: Int) extends Token
  private case class Str(s: String, idx: Int) extends Token
  private case class Name(s: String, idx: Int) extends Token
  private case class Integer(i: Int, idx: Int) extends Token
}
