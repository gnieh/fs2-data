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

package fs2.data
package fst

import transducer.Func
import cats.kernel.BoundedEnumerable
import cats.Id
import cats.Show
import cats.syntax.all._

/** Functions that inject their arguments into the range type
  * or are constant functions (i.e. ignoring their argument).
  */
sealed trait CopyFunc[+T, +C]
object CopyFunc {
  case object CopyArg extends CopyFunc[Nothing, Nothing]
  case class CopyConst[Out](out: Out) extends CopyFunc[Nothing, Out]

  implicit def show[T: Show, C: Show]: Show[CopyFunc[T, C]] = Show.show {
    case CopyFunc.CopyArg        => "<arg>"
    case CopyFunc.CopyConst(out) => out.show
  }

  implicit def CopyFuncChar[X]: Func.Aux[CopyFunc[Char, List[Either[String, X]]], Char, List[Either[String, X]]] =
    new Func[CopyFunc[Char, List[Either[String, X]]]] {
      type Dom = Char
      type Rng = List[Either[String, X]]
      def eval(f: CopyFunc[Char, List[Either[String, X]]])(arg: Dom): Rng =
        f match {
          case CopyFunc.CopyArg        => Left(arg.toString) :: Nil
          case CopyFunc.CopyConst(out) => out
        }

      def isConst(f: CopyFunc[Char, List[Either[String, X]]]): Option[Rng] =
        f match {
          case CopyFunc.CopyArg        => None
          case CopyFunc.CopyConst(out) => Some(out)
        }

      def inDom(t: Char)(f: CopyFunc[Char, List[Either[String, X]]]): Boolean = true

      def domain(f: CopyFunc[Char, List[Either[String, X]]]): LazyList[Char] = BoundedEnumerable[Char].membersAscending

    }

  implicit def CopyFuncEitherListFunc[A, X](implicit
      A: BoundedEnumerable[A]): Func.Aux[CopyFunc[A, List[Either[A, X]]], A, List[Either[A, X]]] =
    new Func[CopyFunc[A, List[Either[A, X]]]] {
      type Dom = A
      type Rng = List[Either[A, X]]

      def eval(f: CopyFunc[A, List[Either[A, X]]])(arg: Dom): Rng =
        f match {
          case CopyArg        => Left(arg) :: Nil
          case CopyConst(out) => out
        }
      def isConst(f: CopyFunc[A, List[Either[A, X]]]): Option[Rng] =
        f match {
          case CopyArg        => None
          case CopyConst(out) => Some(out)
        }
      def inDom(t: Dom)(f: CopyFunc[A, List[Either[A, X]]]): Boolean = true
      def domain(f: CopyFunc[A, List[Either[A, X]]]): LazyList[Dom] = A.membersAscending
    }

  implicit def CopyFuncIdentityFunc[A](implicit
      A: BoundedEnumerable[A]): Func.Aux[CopyFunc[A, List[Id[A]]], A, List[Id[A]]] =
    new Func[CopyFunc[A, List[Id[A]]]] {
      type Dom = A
      type Rng = List[Id[A]]

      def eval(f: CopyFunc[A, List[Id[A]]])(arg: Dom): Rng =
        f match {
          case CopyArg        => List(arg)
          case CopyConst(out) => out
        }
      def isConst(f: CopyFunc[A, List[Id[A]]]): Option[Rng] =
        f match {
          case CopyArg        => None
          case CopyConst(out) => Some(out)
        }
      def inDom(t: Dom)(f: CopyFunc[A, List[Id[A]]]): Boolean = true
      def domain(f: CopyFunc[A, List[Id[A]]]): LazyList[Dom] = A.membersAscending
    }
}
