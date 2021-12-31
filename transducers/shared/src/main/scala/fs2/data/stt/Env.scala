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

import cats.ApplicativeError
import cats.MonadError
import cats.Show
import cats.syntax.all._

import scala.collection.compat._
import scala.reflect.ClassTag

sealed trait Variable
object Variable {
  case class Normal(name: String) extends Variable
  object Normal {
    implicit val show: Show[Normal] = _.name
  }
  case class Stack(name: String) extends Variable

  implicit val show: Show[Variable] = Show.show {
    case Normal(name) => name
    case Stack(name)  => s"${name}ₚ"
  }
}

class Env[V <: Variable, C] private (private val vars: Map[V, Expr[C]]) {

  def widen: Env[Variable, C] =
    new Env(Map.empty[Variable, Expr[C]] ++ vars)

  def stackify(implicit ev: V =:= Variable.Normal): Env[Variable.Stack, C] =
    new Env(vars.collect { case (Variable.Normal(n), e) =>
      (Variable.Stack(n), e)
    })

  def destackify: Env[Variable.Normal, C] =
    new Env(vars.collect { case (v: Variable.Normal, e) =>
      (v, e)
    })

  def merge(that: Env[Variable.Stack, C])(implicit ev: V =:= Variable.Normal): Env[Variable, C] =
    new Env((this.vars ++ that.vars).toMap)

  def call: Env[V, C] =
    new Env[V, C](vars.view.mapValues {
      case Expr0(_) => Expr0.Empty[C]()
      case Expr1(_) => Expr1.Hole[C]()
    }.toMap) {
      override def call = this
    }

  def lookupExpr[F[_]](name: V)(implicit F: ApplicativeError[F, Throwable]): F[Expr[C]] =
    vars.get(name).liftTo[F](STTException(show"unknown varibale $name in environment"))

  def lookup[F[_], E <: Expr[C]](name: V)(implicit F: MonadError[F, Throwable], E: ClassTag[E]): F[E] =
    lookupExpr(name).flatMap {
      case E(e) =>
        F.pure(e)
      case _ =>
        F.raiseError(STTException(show"variable $name is of wrong type"))
    }

  def update(name: V, e: Expr[C]): Env[V, C] =
    new Env(vars.updated(name, e))

}

object Env {
  def create[C](names: Map[String, Type]): Env[Variable.Normal, C] =
    new Env(names.map {
      case (name, Type.Type0) => (Variable.Normal(name), Expr0.Empty[C]())
      case (name, Type.Type1) => (Variable.Normal(name), Expr1.Hole[C]())
    })

  implicit def show[V <: Variable, C: Show]: Show[Env[V, C]] =
    _.vars.map { case (v, e) => show"$v -> $e" }.mkString("\n")
}
