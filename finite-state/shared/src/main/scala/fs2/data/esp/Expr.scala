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

package fs2.data.esp

import cats.Show
import cats.syntax.foldable._
import cats.syntax.show._

sealed trait Expr[+Out]
object Expr {
  case class Call[Out](q: Int, depth: Int, params: List[Expr[Out]], next: Expr[Out]) extends Expr[Out]
  case object Epsilon extends Expr[Nothing]
  case class Open[Out](open: Out, next: Expr[Out]) extends Expr[Out]
  case class Close[Out](close: Out, next: Expr[Out]) extends Expr[Out]
  case class Leaf[Out](value: Out, next: Expr[Out]) extends Expr[Out]
  case class Default[Out](v: Out, next: Expr[Out]) extends Expr[Out]

  def concat[Out](e1: Expr[Out], e2: Expr[Out]): Expr[Out] =
    (e1, e2) match {
      case (Epsilon, _)                => e2
      case (_, Epsilon)                => e1
      case (Call(q, d, p, Epsilon), _) => Call(q, d, p, e2)
      case (Call(q, d, p, e1), _)      => Call(q, d, p, concat(e1, e2))
      case (Default(v, Epsilon), _)    => Default(v, e2)
      case (Default(v, e1), _)         => Default(v, concat(e1, e2))
      case (Open(o, Epsilon), _)       => Open(o, e2)
      case (Open(o, e1), _)            => Open(o, concat(e1, e2))
      case (Close(c, Epsilon), _)      => Close(c, e2)
      case (Close(c, e1), _)           => Close(c, concat(e1, e2))
      case (Leaf(v, Epsilon), _)       => Leaf(v, e2)
      case (Leaf(v, e1), _)            => Leaf(v, concat(e1, e2))
    }

  implicit def show[Out: Show]: Show[Expr[Out]] = Show.show {
    case Call(q, d, ps, next) => show"q${q}_$d(${(ps: List[Expr[Out]]).mkString_(", ")}) $next"
    case Epsilon              => ""
    case Open(o, next)        => show"$o $next"
    case Close(c, next)       => show"$c $next"
    case Leaf(l, next)        => show"$l $next"
    case Default(v, next)     => show"($v)? $next"
  }
}
