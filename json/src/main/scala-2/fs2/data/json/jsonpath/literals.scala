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

import org.typelevel.literally.Literally

import cats.syntax.all._
import cats.data.NonEmptyList

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.unused

object literals {

  implicit class JsonPathStringContext(val sc: StringContext) extends AnyVal {
    def jsonpath(args: Any*): JsonPath = macro JsonPathInterpolator.make
  }

  trait LiftableImpls {
    val c: Context
    import c.universe._

    implicit def nel[T](implicit @unused T: Liftable[T]): Liftable[NonEmptyList[T]] = Liftable[NonEmptyList[T]] {
      case NonEmptyList(t, Nil)  => q"_root_.cats.data.NonEmptyList.one($t)"
      case NonEmptyList(t, tail) => q"_root_.cats.data.NonEmptyList($t, $tail)"
    }

    implicit lazy val predicateLiftable: Liftable[Predicate] = Liftable[Predicate] {
      case Predicate.Index(i)            => q"_root_.fs2.data.json.jsonpath.Predicate.Index($i)"
      case Predicate.Range(lower, upper) => q"_root_.fs2.data.json.jsonpath.Predicate.Range($lower, $upper)"
      case Predicate.Wildcard            => q"_root_.fs2.data.json.jsonpath.Predicate.Wildcard"
    }

    implicit lazy val propertyLiftable: Liftable[Property] = Liftable[Property] {
      case Property.Name(n)  => q"_root_.fs2.data.json.jsonpath.Property.Name($n)"
      case Property.Wildcard => q"_root_.fs2.data.json.jsonpath.Property.Wildcard"
    }

    implicit lazy val locationLiftable: Liftable[Location] = Liftable[Location] {
      case Location.Child(child)     => q"_root_.fs2.data.json.jsonpath.Location.Child($child)"
      case Location.Descendant(desc) => q"_root_.fs2.data.json.jsonpath.Location.Descendant($desc)"
      case Location.Pred(p)          => q"_root_.fs2.data.json.jsonpath.Location.Pred($p)"
    }

    implicit lazy val jsonPathLiftable: Liftable[JsonPath] = Liftable[JsonPath] { case JsonPath(locations) =>
      q"_root_.fs2.data.json.jsonpath.JsonPath($locations)"
    }
  }

  object JsonPathInterpolator extends Literally[JsonPath] {

    def validate(ctx: Context)(string: String): Either[String, ctx.Expr[JsonPath]] = {
      import ctx.universe._
      val liftables = new LiftableImpls {
        val c: ctx.type = ctx
      }
      import liftables._
      JsonPathParser
        .either(string)
        .leftMap(_.getMessage)
        .map(p => c.Expr(q"$p"))
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[JsonPath] = apply(c)(args: _*)

  }
}
