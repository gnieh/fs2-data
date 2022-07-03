package fs2
package data
package xml
package xpath

import automaton.{PDFA, PNFA, Pred}

import Pred.syntax._

import cats.{Eq, Show}
import cats.syntax.all._

case class XPath(locations: List[Location]) {

  def pnfa: PNFA[LocationMatch, StartElement] = {
    def makePredicate(p: Predicate): LocationMatch =
      p match {
        case Predicate.True             => LocationMatch.True
        case Predicate.False            => LocationMatch.False
        case Predicate.Exists(attr)     => LocationMatch.AttrExists(attr)
        case Predicate.Eq(attr, value)  => LocationMatch.AttrEq(attr, value)
        case Predicate.Neq(attr, value) => LocationMatch.AttrNeq(attr, value)
        case Predicate.And(left, right) => makePredicate(left) && makePredicate(right)
        case Predicate.Or(left, right)  => makePredicate(left) || makePredicate(right)
        case Predicate.Not(inner)       => !makePredicate(inner)
      }

    def makeLocation(l: Location): LocationMatch =
      l match {
        case Location(_, n, p) =>
          val node: LocationMatch =
            n match {
              case Node(None, None) => LocationMatch.True
              case _                => LocationMatch.Element(n)
            }
          node && p.map(makePredicate(_)).getOrElse(LocationMatch.True)
      }

    val transitions =
      locations.zipWithIndex.foldLeft(Map.empty[Int, List[(Option[LocationMatch], Int)]]) {
        case (acc, (l @ Location(axis, _, _), idx)) =>
          axis match {
            case Axis.Child => acc.updated(idx, List((Some(makeLocation(l)), idx + 1)))
            case Axis.Descendent =>
              acc.updated(idx, List((Some(makeLocation(l)), idx + 1), (Some(LocationMatch.True), idx)))
          }
      }
    new PNFA(0, Set(transitions.size), transitions)
  }

  def pdfa: PDFA[LocationMatch, StartElement] =
    pnfa.determinize

}

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
  implicit val show: Show[Node] = Show.show {
    case Node(None, None)             => "*"
    case Node(None, Some(local))      => local
    case Node(Some(pfx), None)        => s"$pfx:*"
    case Node(Some(pfx), Some(local)) => s"$pfx:$local"
  }

  implicit val eq: Eq[Node] = Eq.fromUniversalEquals

}

sealed trait Axis
object Axis {
  case object Child extends Axis
  case object Descendent extends Axis
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

}
