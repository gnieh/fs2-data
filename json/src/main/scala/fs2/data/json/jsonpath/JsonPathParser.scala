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
package jsonpath

import cats.MonadError
import cats.data.{NonEmptyList, StateT}
import cats.syntax.all._

import scala.annotation.tailrec

case class JsonPathException(expected: String, got: String, idx: Int)
    extends Exception(s"unexpected '$got' at index $idx, $expected was expected")

class JsonPathParser[F[_]](val input: String)(implicit F: MonadError[F, Throwable]) {

  private type Parser[T] = StateT[F, Int, T]

  private def idx: Parser[Int] =
    StateT.get

  private def modify(f: Int => Int): Parser[Unit] =
    StateT.modify(f)

  private def pure[T](v: T): Parser[T] =
    StateT.pure(v)

  private def tailRecM[A, B](init: A)(f: A => Parser[Either[A, B]]): Parser[B] =
    init.tailRecM(f)

  private def raiseSyntaxError[T](expected: String, got: String): Parser[T] =
    idx.flatMapF(JsonPathException(expected, got, _).raiseError)

  private def accept(c: Char): Parser[Unit] =
    peek.flatMap {
      case Some(`c`)   => consume
      case Some(other) => raiseSyntaxError(c.toString(), other.toString())
      case None        => raiseSyntaxError(c.toString(), "<eos>")
    }

  private val peek: Parser[Option[Char]] =
    idx.map(idx =>
      if (idx >= input.size)
        none
      else
        input.charAt(idx).some)

  private val consume: Parser[Unit] =
    modify(_ + 1)

  def readWhile(pred: Char => Boolean): Parser[String] =
    idx.flatMap { idx =>
      @tailrec
      def loop(idx: Int, builder: StringBuilder): Parser[String] =
        if (idx >= input.size || !pred(input.charAt(idx)))
          modify(_ => idx).as(builder.result())
        else
          loop(idx + 1, builder.append(input.charAt(idx)))
      loop(idx, new StringBuilder)
    }

  private val axis: Parser[Int] =
    peek.flatMap {
      case Some('.') =>
        consume >> peek.flatMap {
          case Some('.') => consume.as(2)
          case _         => pure(1)
        }
      case Some('[') => consume.as(3)
      case Some(c)   => raiseSyntaxError("'.' or '['", c.toString())
      case None      => raiseSyntaxError("'.' or '['", "<eos>")
    }

  private val string: Parser[String] =
    accept('"') >> readWhile(_ != '"') <* accept('"')

  private val int: Parser[Int] =
    readWhile(isDigit(_)).flatMap(s =>
      Either.catchNonFatal(s.toInt).toOption match {
        case Some(i) => pure(i)
        case None    => raiseSyntaxError("integer", s)
      })

  private def isIdentStart(c: Char) =
    c.toString.matches("[a-zA-Z_$]")

  private def isIdentChar(c: Char) =
    c.toString.matches("[a-zA-Z_0-9$-]")

  private def isDigit(c: Char) =
    c >= '0' && c <= '9'

  private val property: Parser[Property] =
    peek.flatMap {
      case Some(c) if isIdentStart(c) => readWhile(isIdentChar(_)).map(Property.Name(_))
      case Some('*')                  => consume.as(Property.Wildcard)
      case Some(c)                    => raiseSyntaxError("identifier of wildcard", c.toString())
      case None                       => raiseSyntaxError("identifier of wildcard", "<eos>")
    }

  private val predicate: Parser[Either[Predicate, Property]] =
    peek.flatMap[Either[Predicate, Property], Int] {
      case Some('"') => string.map(Property.Name(_).asRight)
      case Some(':') => consume >> int.map(i => Predicate.Range(0, i.some).asLeft)
      case Some('*') => consume >> pure(Predicate.Wildcard.asLeft)
      case Some(d) if isDigit(d) =>
        int.flatMap { low =>
          peek.flatMap {
            case Some(':') =>
              consume >> peek.flatMap {
                case Some(c) if isDigit(c) => int.map(high => Predicate.Range(low, high.some).asLeft)
                case _                     => pure(Predicate.Range(low, none).asLeft)
              }
            case _ => pure(Predicate.Index(low).asLeft)
          }
        }
      case Some(c) => raiseSyntaxError("array range or string", c.toString())
      case None    => raiseSyntaxError("array range or string", "<eos>")
    } <* accept(']')

  private val location: Parser[Location] =
    axis.flatMap {
      case 1 => property.map(Location.Child(_))
      case 2 => property.map(Location.Descendant(_))
      case 3 => predicate.map(_.fold(Location.Pred(_), Location.Child(_)))
    }

  def parse(): F[JsonPath] =
    (accept('$') >> location.flatMap { fst =>
      tailRecM(NonEmptyList.one(fst)) { acc =>
        idx.flatMap { idx =>
          if (idx >= input.size)
            pure(JsonPath(acc.reverse).asRight[NonEmptyList[Location]])
          else
            location.map(loc => (loc :: acc).asLeft[JsonPath])
        }
      }
    }).runA(0)

}

object JsonPathParser {
  def apply[F[_]](input: String)(implicit F: MonadError[F, Throwable]): F[JsonPath] =
    new JsonPathParser[F](input).parse()
  // Mostly here for the literal selector macro, makes it simpler (esp with macro+kind-projector interaction)
  def either(input: String): Either[Throwable, JsonPath] = apply[Either[Throwable, *]](input)
}
