/*
 * Copyright 2019-2022 Lucas Satabin
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
package mft

import esp.{ESP, Rhs => ERhs, Rules => ERules}

import cats.syntax.all._

sealed trait Forest {
  def fold[T](children: => T)(siblings: => T): T =
    this match {
      case Forest.Children => children
      case Forest.Siblings => siblings
    }
}
object Forest {
  case object Children extends Forest
  case object Siblings extends Forest
}

sealed trait Pattern[+Tag, +In]
object Pattern {
  case class Node[Tag](tag: Tag) extends Pattern[Tag, Nothing]
  case class Value[In](v: In) extends Pattern[Nothing, In]
  case object Epsilon extends Pattern[Nothing, Nothing]
}

sealed trait Rhs[+Tag, +Out]
object Rhs {
  case class Call[Tag, Out](q: Int, x: Forest, parameters: List[Rhs[Tag, Out]]) extends Rhs[Tag, Out]
  case object Epsilon extends Rhs[Nothing, Nothing]
  case class Param(n: Int) extends Rhs[Nothing, Nothing]
  case class Node[Tag, Out](tag: Tag, children: Rhs[Tag, Out]) extends Rhs[Tag, Out]
  case class Leaf[Out](value: Out) extends Rhs[Nothing, Out]
  case class Concat[Tag, Out](fst: Rhs[Tag, Out], snd: Rhs[Tag, Out]) extends Rhs[Tag, Out]
}

case class Rules[Tag, In, Out](params: List[Int], tree: List[(Pattern[Tag, In], Rhs[Tag, Out])])

trait Conversion[Tag, In, Out] {
  def makeOpenIn(t: Tag): In
  def makeOpenOut(t: Tag): Out
  def makeCloseOut(t: Tag): Out
}

trait TaggedTree[In] {
  def isOpen(in: In): Boolean
  def isClose(in: In): Boolean
}

class MFT[Tag, In, Out](init: Int, rules: Map[Int, Rules[Tag, In, Out]]) {

  def esp[F[_]: RaiseThrowable](implicit In: TaggedTree[In], Tag: Conversion[Tag, In, Out]): ESP[F, In, Out] = {

    def translateRhs(rhs: Rhs[Tag, Out]): ERhs[Out] =
      rhs match {
        case Rhs.Call(q, f, params) => ERhs.Call(q, f.fold(0)(1), params.map(translateRhs(_)))
        case Rhs.Param(i)           => ERhs.Param(i)
        case Rhs.Epsilon            => ERhs.Epsilon
        case Rhs.Node(tag, inner)   => ERhs.Tree(Tag.makeOpenOut(tag), translateRhs(inner), Tag.makeCloseOut(tag))
        case Rhs.Leaf(v)            => ERhs.Leaf(v)
        case Rhs.Concat(rhs1, rhs2) => ERhs.Concat(translateRhs(rhs1), translateRhs(rhs2))
      }

    val erules =
      rules.map { case (q, Rules(params, tree)) =>
        val (qrules, dflt) = tree.foldLeft((Map.empty[Option[(Int, In)], ERhs[Out]], ERhs.epsilon[Out])) {
          case ((acc, dflt), (Pattern.Node(tag), rhs)) =>
            (acc.updated((0, Tag.makeOpenIn(tag)).some, translateRhs(rhs)), dflt)
          case ((acc, dflt), (Pattern.Value(in), rhs)) =>
            (acc.updated((0, in).some, translateRhs(rhs)), dflt)
          case ((acc, _), (Pattern.Epsilon, rhs)) =>
            val erhs = translateRhs(rhs)
            (acc.updated(none, erhs), erhs)
        }
        (q,
         ERules(
           params,
           qrules.withDefault({
             case Some((d, in)) if d > 0 && In.isOpen(in)  => ERhs.Call(q, d + 1, params.map(ERhs.Param(_)))
             case Some((d, in)) if d > 0 && In.isClose(in) => ERhs.Call(q, d - 1, params.map(ERhs.Param(_)))
             case Some((0, in)) if In.isClose(in)          => dflt
             case Some((d, _))                             => ERhs.Call(q, d, params.map(ERhs.Param(_)))
           })
         ))
      }.toMap
    new ESP(init, erules)
  }

}
