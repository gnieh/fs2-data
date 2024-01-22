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
package xml
package xpath

import cats.MonadError
import cats.syntax.all._
import cats.data.StateT
import scala.annotation.tailrec
import cats.data.NonEmptyList

case class XPathSyntaxException(expected: String, got: String, idx: Int)
    extends Exception(s"unexpected '$got' at index $idx, $expected was expected")

class XPathParser[F[_]](val input: String)(implicit F: MonadError[F, Throwable]) {

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
    idx.flatMapF(XPathSyntaxException(expected, got, _).raiseError)

  private def next(expected: String): Parser[Char] =
    idx
      .flatMapF[Char](idx =>
        if (idx >= input.size)
          XPathSyntaxException(expected, "<eos>", idx).raiseError
        else
          input.charAt(idx).pure) <* consume

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

  private val eatSpace: Parser[Unit] =
    idx.flatMap { idx =>
      @tailrec
      def loop(idx: Int): Parser[Unit] =
        if (idx >= input.size || !isXmlWhitespace(input.charAt(idx)))
          modify(_ => idx)
        else
          loop(idx + 1)
      loop(idx)
    }

  def parse(): F[XPath] =
    tailRecM((List.empty[Location], List.empty[List[Location]])) { case (current, acc) =>
      peek.flatMap {
        case None => pure(XPath(NonEmptyList(current.reverse, acc)).asRight[(List[Location], List[List[Location]])])
        case Some('|') => consume >> location.map(loc => (List(loc), current :: acc).asLeft[XPath])
        case _         => location.map(loc => (loc :: current, acc).asLeft[XPath])
      }
    }.runA(0)

  private val axis: Parser[Axis] =
    next("axis").flatMap {
      case '/' =>
        peek.flatMap {
          case Some('/') => consume.as(Axis.Descendant: Axis)
          case _         => pure(Axis.Child: Axis)
        }
      case c =>
        raiseSyntaxError("axis", c.toString())
    }

  private val wildcard: Parser[Node] =
    next("wildcard").flatMap {
      case '*' =>
        peek.flatMap {
          case Some(':') =>
            consume >> ncname.map(n => Node(none, n.some))
          case _ => pure(Node(none, none))
        }
      case c => raiseSyntaxError[Node]("wildcard", c.toString())
    }

  private val ncname: Parser[String] =
    readWhile(isNCNameChar(_))

  private val nameOrWildcard: Parser[Node] =
    ncname.flatMap { n =>
      peek.flatMap {
        case Some(':') =>
          consume >> peek.flatMap {
            case Some('*') =>
              consume.as(Node(n.some, none))
            case Some(c) if isNCNameStart(c) => ncname.map(l => Node(n.some, l.some))
            case Some(c)                     => raiseSyntaxError("NCName or wildcard", c.toString())
            case None                        => raiseSyntaxError("NCName or wildcard", "<eos>")
          }
        case _ => pure(Node(none, n.some))
      }
    }

  private val name: Parser[QName] =
    ncname.flatMap { n =>
      peek.flatMap {
        case Some(':') => consume >> ncname.map(QName(n.some, _))
        case _         => pure(QName(None, n))
      }
    } <* eatSpace

  private val op: Parser[Option[(QName, String) => Predicate]] =
    peek.flatMap {
      case Some('=') => consume >> accept('=').as(((a: QName, v: String) => Predicate.Eq(a, v): Predicate).some)
      case Some('!') => consume >> accept('=').as(((a: QName, v: String) => Predicate.Neq(a, v): Predicate).some)
      case _         => pure(none[(QName, String) => Predicate])
    } <* eatSpace

  private val string: Parser[String] =
    accept('"') >> readWhile(_ != '"') <* accept('"')

  private val atom: Parser[Predicate] =
    next("atom").flatMap {
      case '(' =>
        expr <* accept(')')
      case '@' =>
        name.flatMap(attr =>
          op.flatMap {
            case Some(op) => string.map(op(attr, _))
            case None     => pure(Predicate.Exists(attr): Predicate)
          })
      case '!' => atom.map(Predicate.Not(_): Predicate)
      case c   => raiseSyntaxError[Predicate]("atom", c.toString())
    } <* eatSpace

  private val ands: Parser[Predicate] =
    atom.flatMap { a =>
      peek.flatMap {
        case Some('&') => consume >> accept('&') >> eatSpace >> ands.map(Predicate.And(a, _))
        case _         => pure(a)
      }
    }

  private val expr: Parser[Predicate] =
    ands.flatMap { a =>
      peek.flatMap {
        case Some('|') => consume >> accept('|') >> eatSpace >> expr.map(Predicate.Or(a, _))
        case _         => pure(a)
      }
    }

  private val predicate: Parser[Option[Predicate]] =
    peek.flatMap {
      case Some('[') => consume >> expr.map(_.some) <* accept(']')
      case _         => pure(none)
    }

  private val location: Parser[Location] =
    axis.flatMap { axis =>
      node.flatMap { node =>
        predicate.map { pred =>
          Location(axis, node, pred)
        }
      }
    }

  private val node: Parser[Node] =
    peek.flatMap {
      case Some('*')                   => wildcard
      case Some(c) if isNCNameStart(c) => nameOrWildcard
      case Some(c)                     => raiseSyntaxError("node selector", c.toString())
      case None                        => raiseSyntaxError("node selector", "<eos>")
    }

}

object XPathParser {
  def apply[F[_]](input: String)(implicit F: MonadError[F, Throwable]): F[XPath] =
    new XPathParser[F](input).parse()
  // Mostly here for the literal selector macro, makes it simpler (esp with macro+kind-projector interaction)
  def either(input: String): Either[Throwable, XPath] = apply[Either[Throwable, *]](input)
}
