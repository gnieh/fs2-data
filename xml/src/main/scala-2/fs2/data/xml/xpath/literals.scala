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
package xml
package xpath

import cats.data.NonEmptyList
import cats.syntax.all._

import org.typelevel.literally.Literally

import scala.reflect.macros.blackbox.Context
import scala.annotation.unused

object literals {

  implicit class XPathStringContext(val sc: StringContext) extends AnyVal {
    def xpath(args: Any*): XPath = macro XPathInterpolator.make
  }

  trait LiftableImpls {
    val c: Context
    import c.universe._

    implicit def nel[T](implicit @unused T: Liftable[T]): Liftable[NonEmptyList[T]] =
      Liftable[NonEmptyList[T]] {
        case NonEmptyList(t, Nil)  => q"_root_.cats.data.NonEmptyList.one($t)"
        case NonEmptyList(t, tail) => q"_root_.cats.data.NonEmptyList($t, $tail)"
      }

    implicit lazy val qnameLiftable: Liftable[QName] = Liftable[QName] { n =>
      q"_root_.fs2.data.xml.QName(${n.prefix}, ${n.local})"
    }

    implicit lazy val predicateLiftable: Liftable[Predicate] = Liftable[Predicate] {
      case Predicate.True       => q"_root_.fs2.data.xml.xpath.Predicate.True"
      case Predicate.False      => q"_root_.fs2.data.xml.xpath.Predicate.False"
      case Predicate.Exists(a)  => q"_root_.fs2.data.xml.xpath.Predicate.Exists($a)"
      case Predicate.Eq(a, v)   => q"_root_.fs2.data.xml.xpath.Predicate.Eq($a, $v)"
      case Predicate.Neq(a, v)  => q"_root_.fs2.data.xml.xpath.Predicate.Neq($a, $v)"
      case Predicate.And(l, r)  => q"_root_.fs2.data.xml.xpath.Predicate.And($l, $r)"
      case Predicate.Or(l, r)   => q"_root_.fs2.data.xml.xpath.Predicate.Or($l, $r)"
      case Predicate.Not(inner) => q"_root_.fs2.data.xml.xpath.Predicate.Not($inner)"
    }

    implicit lazy val nodeLiftable: Liftable[Node] = Liftable[Node] { n =>
      q"_root_.fs2.data.xml.xpath.Node(${n.prefix}, ${n.local})"
    }

    implicit lazy val axisLiftable: Liftable[Axis] = Liftable[Axis] {
      case Axis.Child =>
        q"_root_.fs2.data.xml.xpath.Axis.Child"
      case Axis.Descendant =>
        q"_root_.fs2.data.xml.xpath.Axis.Descendant"
    }

    implicit lazy val locationLiftable: Liftable[Location] = Liftable[Location] { l =>
      q"_root_.fs2.data.xml.xpath.Location(${l.axis}, ${l.node}, ${l.predicate})"
    }

    implicit lazy val xpathLiftable: Liftable[XPath] = Liftable[XPath] { p =>
      q"_root_.fs2.data.xml.xpath.XPath(${p.locations})"
    }
  }

  object XPathInterpolator extends Literally[XPath] {

    def validate(ctx: Context)(string: String): Either[String, ctx.Expr[XPath]] = {
      import ctx.universe._
      val liftables = new LiftableImpls {
        val c: ctx.type = ctx
      }
      import liftables._
      XPathParser
        .either(string)
        .leftMap(_.getMessage)
        .map(p => c.Expr(q"$p"))
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[XPath] = apply(c)(args: _*)

  }
}
