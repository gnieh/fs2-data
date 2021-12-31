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

package fs2
package data
package stt

import cats.effect._

import weaver._
import cats.Show

sealed trait Tree
object Tree {
  case class Open(name: String) extends Tree
  case class Close(name: String) extends Tree
  case class Leaf(value: Int) extends Tree

  implicit val hasTags: HasTag[Tree] = {
    case Open(_)  => Tag.Call
    case Close(_) => Tag.Return
    case Leaf(_)  => Tag.Internal
  }

  implicit val show: Show[Tree] = Show.show {
    case Open(name)  => s"<$name"
    case Close(name) => s"$name>"
    case Leaf(v)     => v.toString
  }

}

object STTSpec extends SimpleIOSuite {

  val leaf0: Tree = Tree.Leaf(0)
  val leaf1: Tree = Tree.Leaf(1)
  val openA: Tree = Tree.Open("a")
  val closeA: Tree = Tree.Close("a")
  val openB: Tree = Tree.Open("b")
  val closeB: Tree = Tree.Close("b")

  val x = Variable.Normal("x")
  val y = Variable.Normal("y")
  val z = Variable.Normal("z")
  val xp = Variable.Stack("x")

  test("reverse tree") {

    import Assignment._
    val internalTransition = Map(
      (0, leaf0) -> InternalTransition(0, List(Char(y, leaf0), Prepend(x, y))),
      (0, leaf1) -> InternalTransition(0, List(Char(y, leaf1), Prepend(x, y)))
    )
    val callTransition = Map((0, openA) -> CallTransition[Tree, Tree](0, openA, Nil),
                             (0, openB) -> CallTransition[Tree, Tree](0, openB, Nil))
    val returnTransition = Map(
      (0, openA, closeA) -> ReturnTransition(0, List(Subtree(z, openA, closeA), SubstInY(x, z), Append(x, xp))),
      (0, openB, closeB) -> ReturnTransition(0, List(Subtree(z, openB, closeB), SubstInY(x, z), Append(x, xp)))
    )
    val finalStates = Map(0 -> Expr0.Var[Tree](x))
    val reverse =
      new STT[IO, Tree, Tree, Tree](0,
                                    internalTransition,
                                    callTransition,
                                    returnTransition,
                                    finalStates,
                                    Map("x" -> Type.Type0, "y" -> Type.Type0, "z" -> Type.Type1))

    Stream(openA, openB, leaf0, closeB, leaf1, closeA, openB, closeB)
      .rechunkRandomly()
      .through(reverse)
      .compile
      .toList
      .map(result => expect(result == List(openB, closeB, openA, leaf1, openB, leaf0, closeB, closeA)))
  }

  test("")

}
