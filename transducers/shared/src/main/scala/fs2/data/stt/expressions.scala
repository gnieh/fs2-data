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

sealed trait Type
object Type {
  case object Type0 extends Type
  case object Type1 extends Type
}

sealed trait Expr[C] {
  val tpe: Type
}
object Expr {
  implicit def show[C: Show]: Show[Expr[C]] = Show.show {
    case Expr0(e) => e.show
    case Expr1(e) => e.show
  }
}

sealed trait Expr0[C] extends Expr[C] {
  val tpe = Type.Type0
  def ~(that: Expr0[C]): Expr0[C] = Expr0.Concat(this, that)
}
object Expr0 {
  case class Empty[C]() extends Expr0[C]
  case class Var[C](x: Variable.Normal) extends Expr0[C]
  case class Char[C](c: C) extends Expr0[C]
  case class Subtree[C](open: C, sub: Expr0[C], close: C) extends Expr0[C]
  case class Concat[C](left: Expr0[C], right: Expr0[C]) extends Expr0[C]
  case class Subst[C](inner: Expr1[C], arg: Expr0[C]) extends Expr0[C]

  def unapply[C](e: Expr0[C]): Some[Expr0[C]] =
    Some(e)

  implicit def show[C: Show]: Show[Expr0[C]] = Show.show {
    case Empty()                   => "ε"
    case Var(x)                    => x.show
    case Char(c)                   => c.show
    case Subtree(open, sub, close) => show"⧼$open $sub $close⧽"
    case Concat(left, right)       => show"$left $right"
    case Subst(inner, arg)         => show"$inner[$arg]"
  }
}

sealed trait Expr1[C] extends Expr[C] {
  val tpe = Type.Type1
  def subst(e: Expr0[C]): Expr0[C] =
    this match {
      case Expr1.Hole()                    => e
      case Expr1.Subtree(open, sub, close) => Expr0.Subtree(open, sub.subst(e), close)
      case Expr1.Concat01(left, right)     => Expr0.Concat(left, right.subst(e))
      case Expr1.Concat10(left, right)     => Expr0.Concat(left.subst(e), right)
      case Expr1.Subst(inner, arg)         => inner.subst(arg).subst(e)
    }

  def subst(e: Expr1[C]): Expr1[C] =
    this match {
      case Expr1.Hole()                    => e
      case Expr1.Subtree(open, sub, close) => Expr1.Subtree(open, sub.subst(e), close)
      case Expr1.Concat01(left, right)     => Expr1.Concat01(left, right.subst(e))
      case Expr1.Concat10(left, right)     => Expr1.Concat10(left.subst(e), right)
      case Expr1.Subst(inner, arg)         => inner.subst(arg).subst(e)
    }

}
object Expr1 {
  case class Hole[C]() extends Expr1[C]
  case class Subtree[C](open: C, sub: Expr1[C], close: C) extends Expr1[C]
  case class Concat01[C](left: Expr0[C], right: Expr1[C]) extends Expr1[C]
  case class Concat10[C](left: Expr1[C], right: Expr0[C]) extends Expr1[C]
  case class Subst[C](inner: Expr1[C], arg: Expr1[C]) extends Expr1[C]

  def unapply[C](e: Expr1[C]): Some[Expr1[C]] =
    Some(e)

  implicit def show[C: Show]: Show[Expr1[C]] = Show.show {
    case Hole()                    => "?"
    case Subtree(open, sub, close) => show"⧼$open $sub $close⧽"
    case Concat01(left, right)     => show"$left $right"
    case Concat10(left, right)     => show"$left $right"
    case Subst(inner, arg)         => show"$inner[$arg]"
  }
}
