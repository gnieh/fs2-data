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
package xml
package xpath

import cats.syntax.all.*
import cats.data.NonEmptyList

import org.typelevel.literally.Literally

import scala.language.experimental.macros

import scala.quoted.*

package object literals {

  extension (inline ctx: StringContext) {
    inline def xpath(inline args: Any*): XPath = ${ XPathInterpolator('ctx, 'args) }
  }

  given [T](using ToExpr[T], Type[T]): ToExpr[NonEmptyList[T]] with {
    def apply(nel: NonEmptyList[T])(using Quotes) = nel match {
      case NonEmptyList(t, Nil)  => '{ NonEmptyList.one(${ Expr(t) }) }
      case NonEmptyList(t, tail) => '{ NonEmptyList(${ Expr(t) }, ${ Expr(tail) }) }
    }
  }

  given ToExpr[Node] with {
    def apply(n: Node)(using Quotes) =
      '{ Node(${ Expr(n.prefix) }, ${ Expr(n.local) }) }
  }

  given ToExpr[Axis] with {
    def apply(a: Axis)(using Quotes) =
      a match {
        case Axis.Child      => '{ Axis.Child }
        case Axis.Descendent => '{ Axis.Descendent }
      }
  }

  given ToExpr[QName] with {
    def apply(n: QName)(using Quotes) =
      '{ QName(${ Expr(n.prefix) }, ${ Expr(n.local) }) }
  }

  given ToExpr[Predicate] with {
    def apply(p: Predicate)(using Quotes) =
      p match {
        case Predicate.True       => '{ Predicate.True }
        case Predicate.False      => '{ Predicate.False }
        case Predicate.Exists(a)  => '{ Predicate.Exists(${ Expr(a) }) }
        case Predicate.Eq(a, v)   => '{ Predicate.Eq(${ Expr(a) }, ${ Expr(v) }) }
        case Predicate.Neq(a, v)  => '{ Predicate.Neq(${ Expr(a) }, ${ Expr(v) }) }
        case Predicate.And(l, r)  => '{ Predicate.And(${ Expr(l) }, ${ Expr(r) }) }
        case Predicate.Or(l, r)   => '{ Predicate.Or(${ Expr(l) }, ${ Expr(r) }) }
        case Predicate.Not(inner) => '{ Predicate.Not(${ Expr(inner) }) }
      }
  }

  given ToExpr[Location] with {
    def apply(l: Location)(using Quotes) =
      '{ Location(${ Expr(l.axis) }, ${ Expr(l.node) }, ${ Expr(l.predicate) }) }
  }

  given ToExpr[XPath] with {
    def apply(p: XPath)(using Quotes) =
      '{ XPath(${ Expr(p.locations) }) }
  }
  object XPathInterpolator extends Literally[XPath] {

    def validate(string: String)(using Quotes) = {
      XPathParser
        .either(string)
        .leftMap(_.getMessage)
        .map(Expr(_))
    }

  }
}
