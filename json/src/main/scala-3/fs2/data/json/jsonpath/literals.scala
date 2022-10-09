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
package jsonpath

import cats.syntax.all.*
import cats.data.NonEmptyList

import scala.quoted.*

import org.typelevel.literally.Literally

import scala.language.experimental.macros

package object literals {

  extension (inline ctx: StringContext) {
    inline def jsonpath(inline args: Any*): JsonPath = ${ JsonPathInterpolator('ctx, 'args) }
  }

  given [T](using ToExpr[T], Type[T]): ToExpr[NonEmptyList[T]] with {
    def apply(nel: NonEmptyList[T])(using Quotes) = nel match {
      case NonEmptyList(t, Nil)  => '{ NonEmptyList.one(${ Expr(t) }) }
      case NonEmptyList(t, tail) => '{ NonEmptyList(${ Expr(t) }, ${ Expr(tail) }) }
    }
  }

  given ToExpr[Predicate] with {
    def apply(p: Predicate)(using Quotes) =
      p match {
        case Predicate.Index(i)    => '{ Predicate.Index(${ Expr(i) }) }
        case Predicate.Range(l, u) => '{ Predicate.Range(${ Expr(l) }, ${ Expr(u) }) }
        case Predicate.Wildcard    => '{ Predicate.Wildcard }
      }
  }

  given ToExpr[Property] with {
    def apply(p: Property)(using Quotes) =
      p match {
        case Property.Name(n)  => '{ Property.Name(${ Expr(n) }) }
        case Property.Wildcard => '{ Property.Wildcard }
      }
  }

  given ToExpr[Location] with {
    def apply(l: Location)(using Quotes) =
      l match {
        case Location.Child(c)      => '{ Location.Child(${ Expr(c) }) }
        case Location.Descendant(d) => '{ Location.Descendant(${ Expr(d) }) }
        case Location.Pred(p)       => '{ Location.Pred(${ Expr(p) }) }
      }
  }

  given ToExpr[JsonPath] with {
    def apply(p: JsonPath)(using Quotes) =
      '{ JsonPath(${ Expr(p.locations) }) }
  }

  object JsonPathInterpolator extends Literally[JsonPath] {

    def validate(string: String)(using Quotes) = {
      JsonPathParser
        .either(string)
        .leftMap(_.getMessage)
        .map(Expr(_))
    }

  }
}
