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

package fs2
package data
package json
package jq

import cats.data.NonEmptyList
import cats.syntax.all._
import org.typelevel.literally.Literally

import scala.annotation.unused
import scala.reflect.macros.blackbox.Context

object literals {

  implicit class JqStringContext(val sc: StringContext) extends AnyVal {
    def jq(args: Any*): Jq = macro JqInterpolator.make
  }

  trait LiftableImpls {
    val c: Context
    import c.universe._

    implicit def nel[T](implicit @unused T: Liftable[T]): Liftable[NonEmptyList[T]] = Liftable[NonEmptyList[T]] {
      case NonEmptyList(t, Nil)  => q"_root_.cats.data.NonEmptyList.one($t)"
      case NonEmptyList(t, tail) => q"_root_.cats.data.NonEmptyList($t, $tail)"
    }

    implicit lazy val jqLiftable: Liftable[Jq] = Liftable[Jq] {
      case Jq.Root              => q"_root_.fs2.data.json.jq.Jq.Root"
      case Jq.Identity          => q"_root_.fs2.data.json.jq.Jq.Identity"
      case Jq.Field(name)       => q"_root_.fs2.data.json.jq.Jq.Field($name)"
      case Jq.Index(idx)        => q"_root_.fs2.data.json.jq.Jq.Index($idx)"
      case Jq.Slice(idx1, idx2) => q"_root_.fs2.data.json.jq.Jq.Slice($idx1, $idx2)"
      case Jq.RecursiveDescent  => q"_root_.fs2.data.json.jq.Jq.RecursiveDescent"
      case Jq.Sequence(qs) =>
        q"_root_.fs2.data.json.jq.Jq.Sequence(_root_.cats.data.NonEmptyChain.fromNonEmptyList(${qs.toNonEmptyList.widen[Jq]}))"
      case Jq.Iterator(filter, inner) => q"_root_.fs2.data.json.jq.Jq.Iterator(${filter: Jq}, $inner)"
      case Jq.Arr(pfx, qs)            => q"_root_.fs2.data.json.jq.Jq.Arr(${pfx: Jq}, $qs)"
      case Jq.Obj(pfx, qs)            => q"_root_.fs2.data.json.jq.Jq.Obj(${pfx: Jq}, $qs)"
      case Jq.Num(n)                  => q"_root_.fs2.data.json.jq.Jq.Num($n)"
      case Jq.Str(s)                  => q"_root_.fs2.data.json.jq.Jq.Str($s)"
      case Jq.Bool(b)                 => q"_root_.fs2.data.json.jq.Jq.Bool($b)"
      case Jq.Null                    => q"_root_.fs2.data.json.jq.Jq.Null"
    }
  }

  object JqInterpolator extends Literally[Jq] {

    def validate(ctx: Context)(string: String): Either[String, ctx.Expr[Jq]] = {
      import ctx.universe._
      val liftables = new LiftableImpls {
        val c: ctx.type = ctx
      }
      import liftables._
      JqParser
        .either(string)
        .leftMap(_.getMessage)
        .map(p => c.Expr(q"$p"))
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[Jq] = apply(c)(args: _*)

  }
}
