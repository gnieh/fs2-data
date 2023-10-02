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
package internals

import pfsa._

import cats.syntax.all._
import cats.data.NonEmptyChain

sealed trait LocationMatch
object LocationMatch {
  case object True extends LocationMatch
  case object False extends LocationMatch

  case class Element(name: Node) extends LocationMatch

  case class AttrExists(attr: QName) extends LocationMatch
  case class AttrEq(attr: QName, value: String) extends LocationMatch
  case class AttrNeq(attr: QName, value: String) extends LocationMatch

  case class And(left: LocationMatch, right: LocationMatch) extends LocationMatch
  case class Or(left: LocationMatch, right: LocationMatch) extends LocationMatch
  case class Not(inner: LocationMatch) extends LocationMatch

  implicit val LocationMatchPred: Pred[LocationMatch, StartElement] = new Pred[LocationMatch, StartElement] {

    override def satisfies(p: LocationMatch)(e: StartElement): Boolean =
      p match {
        case True                 => true
        case False                => false
        case Element(node)        => node.matches(e.name)
        case AttrExists(attr)     => e.attributes.contains(attr)
        case AttrEq(attr, value)  => e.attributes.get(attr).contains(value)
        case AttrNeq(attr, value) => e.attributes.get(attr).fold(false)(_ =!= value)
        case And(left, right)     => satisfies(left)(e) && satisfies(right)(e)
        case Or(left, right)      => satisfies(left)(e) || satisfies(right)(e)
        case Not(inner)           => !satisfies(inner)(e)
      }

    override def always: LocationMatch = True

    override def never: LocationMatch = False

    override def and(p1: LocationMatch, p2: LocationMatch): LocationMatch =
      (p1, p2) match {
        case (True, _)                                      => p2
        case (_, True)                                      => p1
        case (False, _)                                     => False
        case (_, False)                                     => False
        case (Element(n1), Element(n2)) if (n1 === n2)      => p1
        case (Element(n1), Element(n2)) if (n1 =!= n2)      => False
        case (Not(Element(n1)), Element(n2)) if (n1 === n2) => False
        case (Not(Element(n1)), Element(n2)) if (n1 =!= n2) => p2
        case (Element(n1), Not(Element(n2))) if (n1 === n2) => False
        case (Element(n1), Not(Element(n2))) if (n1 =!= n2) => p1
        case (_, _)                                         => And(p1, p2)
      }

    override def or(p1: LocationMatch, p2: LocationMatch): LocationMatch =
      (p1, p2) match {
        case (True, _)  => True
        case (_, True)  => True
        case (False, _) => p2
        case (_, False) => p1
        case (_, _)     => Or(p1, p2)
      }

    override def not(p: LocationMatch): LocationMatch =
      p match {
        case True       => False
        case False      => True
        case Not(inner) => inner
        case _          => Not(p)
      }

    override def isSatisfiable(p: LocationMatch): Boolean =
      p != False

  }

}

case class StartElement(name: QName, attributes: Map[QName, String])

sealed trait Atom {
  def satisfies(e: StartElement): Boolean
}
object Atom {
  case object True extends Atom {
    override def satisfies(e: StartElement): Boolean = true
  }
  case object False extends Atom {
    override def satisfies(e: StartElement): Boolean = false
  }

  case class Element(name: QName) extends Atom {
    override def satisfies(e: StartElement): Boolean = e.name === name
  }

  case class AttrExists(attr: QName) extends Atom {
    override def satisfies(e: StartElement): Boolean = e.attributes.contains(attr)
  }
  case class AttrEq(attr: QName, value: String) extends Atom {
    override def satisfies(e: StartElement): Boolean = e.attributes.get(attr).contains(value)
  }
  case class AttrNeq(attr: QName, value: String) extends Atom {
    override def satisfies(e: StartElement): Boolean = !e.attributes.get(attr).contains(value)
  }

  case class Not(inner: Atom) extends Atom {
    override def satisfies(e: StartElement): Boolean = !inner.satisfies(e)
  }
}

case class Disjunction(atoms: NonEmptyChain[Atom]) {
  def satisfies(e: StartElement): Boolean =
    atoms.exists(_.satisfies(e))
}

case class CNF(clauses: NonEmptyChain[Disjunction]) {
  def satisfies(e: StartElement): Boolean =
    clauses.forall(_.satisfies(e))
}
