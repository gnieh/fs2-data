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

package fs2.data.pfsa

import cats.data.Chain
import cats.syntax.all._
import cats.{Eq, Show}

import Pred.syntax._

/** Simple regular language with character sets.
  * This allows to model simple query languages (think XPath or JsonPath)
  * and derive DFA out of it.
  */
sealed abstract class Regular[CharSet] {

  def ~(that: Regular[CharSet])(implicit CharSet: Pred[CharSet, _], eq: Eq[CharSet]): Regular[CharSet] =
    (this, that) match {
      case (Regular.Epsilon(), _)               => that
      case (_, Regular.Epsilon())               => this
      case (Regular.Concatenation(re1, re2), _) => re1 ~ (re2 ~ that)
      case (_, _) =>
        if (this.isSatisfiable && that.isSatisfiable)
          Regular.Concatenation(this, that)
        else
          Regular.empty
    }

  def ?(implicit CharSet: Pred[CharSet, _], eq: Eq[CharSet]): Regular[CharSet] =
    this || Regular.empty

  def &&(that: Regular[CharSet])(implicit CharSet: Pred[CharSet, _], eq: Eq[CharSet]): Regular[CharSet] =
    (this, that) match {
      case (Regular.And(re1, re2), _) => re1 && (re2 && that)
      case (_, _) =>
        if (this === that)
          this
        else if (this === Regular.any)
          that
        else if (that === Regular.any)
          this
        else if (this.isSatisfiable && that.isSatisfiable)
          Regular.And(this, that)
        else
          Regular.empty
    }

  def ||(that: Regular[CharSet])(implicit CharSet: Pred[CharSet, _], eq: Eq[CharSet]): Regular[CharSet] =
    (this, that) match {
      case (Regular.Or(re1, re2), _)                => re1 || (re2 || that)
      case (Regular.Chars(cs1), Regular.Chars(cs2)) => Regular.Chars(cs1 || cs2)
      case (_, _) =>
        if (this === that)
          this
        else if (this === Regular.any)
          Regular.any
        else if (that === Regular.any)
          Regular.any
        else if (!this.isSatisfiable)
          that
        else if (!that.isSatisfiable)
          this
        else
          Regular.Or(this, that)
    }

  def unary_!(implicit CharSet: Pred[CharSet, _], eq: Eq[CharSet]): Regular[CharSet] =
    this match {
      case Regular.Not(re)   => re
      case Regular.Chars(cs) => Regular.Chars(!cs)
      case Regular.Epsilon() => Regular.any
      case _ =>
        if (this === Regular.any)
          Regular.empty
        else if (this === Regular.empty)
          Regular.any
        else
          Regular.Not(this)
    }

  def rep(implicit CharSet: Pred[CharSet, _], eq: Eq[CharSet]): Regular[CharSet] =
    this match {
      case Regular.Star(_) => this
      case _ =>
        if (this === Regular.epsilon)
          Regular.epsilon
        else if (this === Regular.empty)
          Regular.epsilon
        else
          Regular.Star(this)

    }

  def acceptEpsilon: Boolean =
    this match {
      case Regular.Epsilon()               => true
      case Regular.Star(_)                 => true
      case Regular.Or(re1, re2)            => re1.acceptEpsilon || re2.acceptEpsilon
      case Regular.And(re1, re2)           => re1.acceptEpsilon && re2.acceptEpsilon
      case Regular.Concatenation(re1, re2) => re1.acceptEpsilon && re2.acceptEpsilon
      case Regular.Chars(_)                => false
      case Regular.Not(re)                 => !re.acceptEpsilon
    }

  def derive[C](c: C)(implicit CharSet: Pred[CharSet, C], eq: Eq[CharSet]): Regular[CharSet] =
    this match {
      case Regular.Epsilon()                               => Regular.Chars(CharSet.never)
      case Regular.Chars(set) if CharSet.satisfies(set)(c) => Regular.Epsilon()
      case Regular.Chars(_)                                => Regular.Chars(CharSet.never)
      case Regular.Concatenation(re1, re2) if re1.acceptEpsilon =>
        (re1.derive(c) ~ re2) || re2.derive(c)
      case Regular.Concatenation(re1, re2) =>
        re1.derive(c) ~ re2
      case Regular.Or(re1, re2) =>
        re1.derive(c) || re2.derive(c)
      case Regular.And(re1, re2) =>
        re1.derive(c) && re2.derive(c)
      case Regular.Star(re) =>
        re.derive(c) ~ Regular.Star(re)
      case Regular.Not(re) =>
        !re.derive(c)
    }

  def classes[C](implicit CharSet: Pred[CharSet, C]): Set[CharSet] =
    this match {
      case Regular.Epsilon()                                    => Set(CharSet.always)
      case Regular.Chars(chars)                                 => Set(chars, CharSet.not(chars))
      case Regular.Concatenation(re1, re2) if re1.acceptEpsilon => combine(re1.classes, re2.classes)
      case Regular.Concatenation(re1, _)                        => re1.classes
      case Regular.Or(re1, re2)                                 => combine(re1.classes, re2.classes)
      case Regular.And(re1, re2)                                => combine(re1.classes, re2.classes)
      case Regular.Star(re)                                     => re.classes
      case Regular.Not(re)                                      => re.classes
    }

  private def combine[C](c1: Set[CharSet], c2: Set[CharSet])(implicit CharSet: Pred[CharSet, C]): Set[CharSet] =
    for {
      cs1 <- c1
      cs2 <- c2
      both = CharSet.and(cs1, cs2)
      if CharSet.isSatisfiable(both)
    } yield both

  def deriveDFA[C](implicit
      CharSet: Pred[CharSet, C],
      candidate: Candidate[CharSet, C],
      eq: Eq[CharSet]): PDFA[CharSet, C] = {

    def goto(re: Regular[CharSet],
             q: Int,
             cs: CharSet,
             qs: Chain[Regular[CharSet]],
             transitions: Map[Int, List[(CharSet, Int)]]): (Chain[Regular[CharSet]], Map[Int, List[(CharSet, Int)]]) =
      candidate.pick(cs) match {
        case Some(c) =>
          val tgt = re.derive(c)
          val equivalent = qs.zipWithIndex.collectFirst {
            case (q, idx) if tgt === q => idx
          }
          equivalent match {
            case Some(tgt) => (qs, transitions.combine(Map(q -> List(cs -> tgt))))
            case None =>
              val qs1 = qs.append(tgt)
              val q1 = qs.size.toInt
              val transitions1 = transitions.combine(Map(q -> List(cs -> q1)))
              explore(qs1, transitions1, tgt)
          }
        case None =>
          (qs, transitions)
      }

    def explore(qs: Chain[Regular[CharSet]],
                transitions: Map[Int, List[(CharSet, Int)]],
                re: Regular[CharSet]): (Chain[Regular[CharSet]], Map[Int, List[(CharSet, Int)]]) = {
      val q = qs.size.toInt - 1
      val cls = re.classes
      cls.foldLeft((qs, transitions)) { case ((qs, transitions), cs) =>
        goto(re, q, cs, qs, transitions)
      }
    }

    val (qs, transitions) = explore(Chain.one(this), Map.empty, this)
    val indexedStates = qs.zipWithIndex
    val finals = indexedStates.collect { case (re, idx) if re.acceptEpsilon => idx }.toList.toSet
    val trap = indexedStates.collectFirst { case (Regular.Chars(cs), idx) if cs === never => idx }
    new PDFA[CharSet, C](0, finals, trap, Array.tabulate(qs.size.toInt)(transitions.getOrElse(_, Nil)))
  }

}
object Regular {
  private case class Epsilon[CharSet]() extends Regular[CharSet]
  private case class Chars[CharSet](set: CharSet) extends Regular[CharSet]
  private case class Star[CharSet](re: Regular[CharSet]) extends Regular[CharSet]
  private case class Concatenation[CharSet](re1: Regular[CharSet], re2: Regular[CharSet]) extends Regular[CharSet]
  private case class Or[CharSet](re1: Regular[CharSet], re2: Regular[CharSet]) extends Regular[CharSet]
  private case class And[CharSet](re1: Regular[CharSet], re2: Regular[CharSet]) extends Regular[CharSet]
  private case class Not[CharSet](re: Regular[CharSet]) extends Regular[CharSet]

  private[pfsa] def eq[CharSet: Eq]: Eq[Regular[CharSet]] = regeq[CharSet]

  implicit def regeq[CharSet: Eq]: Eq[Regular[CharSet]] = Eq.instance {
    case (Epsilon(), Epsilon())                                 => true
    case (Chars(cs1), Chars(cs2))                               => cs1 === cs2
    case (Star(re1), Star(re2))                                 => re1 === re2
    case (Concatenation(re11, re12), Concatenation(re21, re22)) => re11 === re21 && re12 === re22
    case (Or(re11, re12), Or(re21, re22)) =>
      (re11 === re21 && re12 === re22) || (re11 === re22 && re12 === re21)
    case (And(re11, re12), And(re21, re22)) =>
      (re11 === re21 && re12 === re22) || (re11 === re22 && re12 === re21)
    case (Not(re1), Not(re2)) => re1 === re2
    case _                    => false
  }

  def epsilon[CharSet]: Regular[CharSet] = Epsilon()

  def chars[CharSet](cs: CharSet): Regular[CharSet] =
    Regular.Chars(cs)

  def any[CharSet](implicit CharSet: Pred[CharSet, _]): Regular[CharSet] = Chars(CharSet.always)

  def empty[CharSet](implicit CharSet: Pred[CharSet, _]): Regular[CharSet] = Chars(CharSet.never)

  implicit def pred[CharSet: Eq, C](implicit CharSet: Pred[CharSet, C]): Pred[Regular[CharSet], C] =
    new Pred[Regular[CharSet], C] {

      override def satisfies(p: Regular[CharSet])(e: C): Boolean =
        p match {
          case Epsilon()  => false
          case Chars(set) => set.satisfies(e)
          case Star(re)   => re.satisfies(e)
          case Concatenation(re1, re2) =>
            re1.satisfies(e) || (re1.acceptEpsilon && re2.satisfies(e))
          case Or(re1, re2)  => re1.satisfies(e) || re2.satisfies(e)
          case And(re1, re2) => re1.satisfies(e) && re2.satisfies(e)
          case Not(re)       => !re.satisfies(e)
        }

      override def always: Regular[CharSet] = any

      override def never: Regular[CharSet] = empty

      override def and(p1: Regular[CharSet], p2: Regular[CharSet]): Regular[CharSet] = p1 && p2

      override def or(p1: Regular[CharSet], p2: Regular[CharSet]): Regular[CharSet] = p1 || p2

      override def not(p: Regular[CharSet]): Regular[CharSet] = !p

      override def isSatisfiable(p: Regular[CharSet]): Boolean = p =!= empty

    }

  implicit def show[CS: Show]: Show[Regular[CS]] = Show.show {
    case Epsilon()               => "ε"
    case Chars(cs)               => cs.show
    case Concatenation(re1, re2) => show"$re1$re2"
    case Or(re1, re2)            => show"($re1) | ($re2)"
    case And(re1, re2)           => show"($re1) & ($re2)"
    case Star(re)                => show"($re)*"
    case Not(re)                 => show"~($re)"
  }
}
