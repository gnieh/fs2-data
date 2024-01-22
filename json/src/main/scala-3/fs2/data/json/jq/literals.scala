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
package json
package jq

import cats.syntax.all.*
import cats.data.{NonEmptyChain, NonEmptyList}

import scala.quoted.*

import org.typelevel.literally.Literally

import scala.language.experimental.macros

package object literals {

  extension (inline ctx: StringContext) {
    inline def jq(inline args: Any*): Jq = ${ JqInterpolator('ctx, 'args) }
  }

  given [T](using ToExpr[T], Type[T]): ToExpr[NonEmptyList[T]] with {
    def apply(nel: NonEmptyList[T])(using Quotes) = nel match {
      case NonEmptyList(t, Nil)  => '{ NonEmptyList.one(${ Expr(t) }) }
      case NonEmptyList(t, tail) => '{ NonEmptyList(${ Expr(t) }, ${ Expr(tail) }) }
    }
  }

  given ToExpr[SimpleFilter] with {
    def apply(f: SimpleFilter)(using Quotes) =
      f match {
        case Jq.Root              => '{ Jq.Root }
        case Jq.Identity          => '{ Jq.Identity }
        case Jq.Field(name)       => '{ Jq.Field(${ Expr(name) }) }
        case Jq.Index(idx)        => '{ Jq.Index(${ Expr(idx) }) }
        case Jq.Slice(idx1, idx2) => '{ Jq.Slice(${ Expr(idx1) }, ${ Expr(idx2) }) }
        case Jq.Child             => '{ Jq.Child }
        case Jq.RecursiveDescent  => '{ Jq.RecursiveDescent }
      }
  }

  given ToExpr[Filter] with {
    def apply(f: Filter)(using Quotes) =
      f match {
        case Jq.Sequence(qs) =>
          '{ Jq.Sequence(NonEmptyChain.fromNonEmptyList(${ Expr(qs.toNonEmptyList) })) }
        case simple: SimpleFilter => Expr(simple)
      }
  }

  given ToExpr[Jq] with {
    def apply(q: Jq)(using Quotes) =
      q match {
        case f: Filter                  => Expr(f)
        case Jq.Iterator(filter, inner) => '{ Jq.Iterator(${ Expr(filter) }, ${ Expr(inner) }) }
        case Jq.Arr(pfx, qs)            => '{ Jq.Arr(${ Expr(pfx) }, ${ Expr(qs) }) }
        case Jq.Obj(pfx, qs)            => '{ Jq.Obj(${ Expr(pfx) }, ${ Expr(qs) }) }
        case Jq.Num(n)                  => '{ Jq.Num(${ Expr(n) }) }
        case Jq.Str(s)                  => '{ Jq.Str(${ Expr(s) }) }
        case Jq.Bool(b)                 => '{ Jq.Bool(${ Expr(b) }) }
        case Jq.Null                    => '{ Jq.Null }
      }
  }

  object JqInterpolator extends Literally[Jq] {

    def validate(string: String)(using Quotes) = {
      JqParser
        .either(string)
        .leftMap(_.getMessage)
        .map(Expr(_))
    }

  }
}
