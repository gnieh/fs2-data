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

import cats.Eq
import cats.syntax.all._
import cats.data.NonEmptyList
import cats.Show
import scala.runtime.AbstractFunction1
import scala.runtime.AbstractFunction3

case class XPath(locations: NonEmptyList[List[Location]])

object XPath extends AbstractFunction1[NonEmptyList[List[Location]], XPath] {

  private implicit val showLocation: Show[List[Location]] =
    _.mkString_("")

  implicit val show: Show[XPath] =
    _.locations.mkString_("|")

}

case class Location(axis: Axis, node: Node, predicate: Option[Predicate])

object Location extends AbstractFunction3[Axis, Node, Option[Predicate], Location] {

  private implicit val showPredicats: Show[Option[Predicate]] =
    _.fold("")(_.show)

  implicit val show: Show[Location] = loc => show"${loc.axis}${loc.node}${loc.predicate}"
}

case class Node(prefix: Option[String], local: Option[String]) {

  def matches(name: QName): Boolean =
    this match {
      case Node(None, None)        => true
      case Node(None, Some(local)) => name.local === local
      case Node(pfx, None)         => name.prefix === pfx
      case Node(pfx, Some(local))  => name.prefix === pfx && name.local === local
    }
}
object Node extends ((Option[String], Option[String]) => Node) {
  implicit val eq: Eq[Node] = Eq.fromUniversalEquals

  implicit val show: Show[Node] = _ match {
    case Node(None, None)                => "*"
    case Node(None, Some(local))         => local
    case Node(Some(prefix), None)        => s"$prefix:*"
    case Node(Some(prefix), Some(local)) => s"$prefix:$local"
  }
}

sealed trait Axis
object Axis {
  case object Child extends Axis
  case object Descendant extends Axis

  implicit val show: Show[Axis] = _ match {
    case Child      => "/"
    case Descendant => "//"
  }
}

sealed trait Predicate
object Predicate {
  case object True extends Predicate
  case object False extends Predicate
  case class Exists(attr: QName) extends Predicate
  case class Eq(attr: QName, value: String) extends Predicate
  case class Neq(attr: QName, value: String) extends Predicate

  case class And(left: Predicate, right: Predicate) extends Predicate
  case class Or(left: Predicate, right: Predicate) extends Predicate
  case class Not(inner: Predicate) extends Predicate

  implicit val show: Show[Predicate] = _ match {
    case True             => "true"
    case False            => "false"
    case Exists(attr)     => show"@$attr"
    case Eq(attr, value)  => show"""@$attr == "$value""""
    case Neq(attr, value) => show"""@$attr != "$value""""
    case And(left, right) => s"${showAnd(left)} && ${showAnd(right)}"
    case Or(left, right)  => show"$left || $right"
    case Not(inner)       => s"!${showNot(inner)}"
  }

  private def showAnd(p: Predicate): String = p match {
    case Or(_, _) => show"($p)"
    case _        => p.show
  }

  private def showNot(p: Predicate): String = p match {
    case Or(_, _) | And(_, _) => show"($p)"
    case _                    => p.show
  }
}
