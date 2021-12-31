/*
 * Copyright 2021 Lucas Satabin
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

package fs2.data.stt

import cats.Show
import cats.syntax.show._

sealed trait Assignment[+V <: Variable, +C]
sealed trait Reset[+C] extends Assignment[Nothing, C]
object Reset {
  def unapply[C](r: Reset[C]): Some[Reset[C]] =
    Some(r)
}
object Assignment {
  case class Empty(x: Variable.Normal) extends Reset[Nothing]
  case class Hole(x: Variable.Normal) extends Reset[Nothing]
  case class Char[C](x: Variable.Normal, c: C) extends Reset[C]
  case class Subtree[C](x: Variable.Normal, open: C, close: C) extends Reset[C]
  case class Append[V <: Variable](x: Variable.Normal, y: V) extends Assignment[V, Nothing]
  case class Prepend[V <: Variable](x: Variable.Normal, y: V) extends Assignment[V, Nothing]
  case class SubstInX[V <: Variable](x: Variable.Normal, y: V) extends Assignment[V, Nothing]
  case class SubstInY[V <: Variable](x: Variable.Normal, y: V) extends Assignment[V, Nothing]
  case class Swap[V <: Variable](x: Variable.Normal, y: V) extends Assignment[V, Nothing]

  implicit def show[V <: Variable, C: Show]: Show[Assignment[V, C]] = Show.show {
    case Empty(x)                => show"$x := ε;"
    case Hole(x)                 => show"$x := ?;"
    case Char(x, c)              => show"$x := $c;"
    case Subtree(x, open, close) => show"$x := ⧼$open ? $close⧽;"
    case Append(x, y) => show"""$x := $x $y;
                               |$y := ε;""".stripMargin
    case Prepend(x, y) => show"""$x := $y $x;
                                |$y := ε;""".stripMargin
    case SubstInX(x, y) => show"""$x := $x[$y];
                                 |$y := ε;""".stripMargin
    case SubstInY(x, y) => show"""$x := $y[$x];
                                 |$y := ε;""".stripMargin
    case Swap(x, y) => show"""$x := $y;
                             |$y := $x;""".stripMargin
  }

}
