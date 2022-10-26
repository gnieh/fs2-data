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
package xml
package xpath

import cats.{Eq, Show}
import cats.syntax.all._
import cats.data.NonEmptyList

case class XPath(locations: NonEmptyList[List[Location]])

case class Location(axis: Axis, node: Node, predicate: Option[Predicate])

case class Node(prefix: Option[String], local: Option[String]) {

  def matches(name: QName): Boolean =
    this match {
      case Node(None, None)        => true
      case Node(None, Some(local)) => name.local === local
      case Node(pfx, None)         => name.prefix === pfx
      case Node(pfx, Some(local))  => name.prefix === pfx && name.local === local
    }
}
object Node {
  implicit val eq: Eq[Node] = Eq.fromUniversalEquals
}

sealed trait Axis
object Axis {
  case object Child extends Axis
  case object Descendant extends Axis
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

  implicit val show: Show[Predicate] = Show.show {
    case True        => "true"
    case False       => "false"
    case Exists(a)   => s"@${a.render}"
    case Eq(a, v)    => s"""@${a.render}="$v""""
    case Neq(a, v)   => s"""@${a.render}!="$v""""
    case And(p1, p2) => show"${render(1, p1)} && ${render(1, p2)}"
    case Or(p1, p2)  => show"${render(2, p1)} || ${render(2, p2)}"
    case Not(p)      => show"!${render(0, p)}"
  }

  private def render(lvl: Int, p: Predicate): String =
    p match {
      case And(p1, p2) =>
        if (lvl < 1)
          s"(${render(1, p1)} && ${render(1, p2)})"
        else
          s"${render(1, p1)} && ${render(1, p2)}"
      case Or(p1, p2) =>
        if (lvl < 2)
          s"(${render(2, p1)} || ${render(2, p2)})"
        else
          s"${render(2, p1)} || ${render(2, p2)}"
      case Not(p) =>
        s"!${render(0, p)}"
      case _ =>
        p.show
    }

}
