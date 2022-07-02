package fs2
package data
package xml
package xpath

import automaton.{DFA, NFA, Symbol}

case class XPath(locations: List[Location]) {

  def dfa: DFA[QName] = {
    val transitions =
      locations.zipWithIndex.foldLeft(Map.empty[Int, List[(Symbol[QName], Int)]]) {
        case (acc, (Location(axis, name), idx)) =>
          axis match {
            case Axis.Child      => acc.updated(idx, List((Symbol.Sym(name), idx + 1)))
            case Axis.Descendent => acc.updated(idx, List((Symbol.Sym(name), idx + 1), (Symbol.Any, idx)))
          }
      }
    new NFA(0, Set(transitions.size), transitions).determinize
  }

}

case class Location(axis: Axis, name: QName)

sealed trait Axis
object Axis {
  case object Child extends Axis
  case object Descendent extends Axis
}
