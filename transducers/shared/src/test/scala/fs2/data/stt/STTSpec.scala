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

import weaver._
import cats.Show
import cats.effect.IO

sealed trait Tree
object Tree {
  case class Open(name: String) extends Tree
  case class Close(name: String) extends Tree
  case class Leaf(value: Int) extends Tree

  implicit val show: Show[Tree] = Show.show {
    case Open(name)  => s"<$name"
    case Close(name) => s"$name>"
    case Leaf(v)     => v.toString
  }

  implicit val hasTags: HasTag[Tree] = {
    case Open(_)  => Tag.Call
    case Close(_) => Tag.Return
    case Leaf(_)  => Tag.Internal
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

  test("reverse tree (map)") {

    import Assignment._
    val internalTransition =
      Map[(Int, Tree), InternalTransition[Tree]]((0, leaf0) ->
                                                   InternalTransition(0, List(Char(y, leaf0), Prepend(x, y))),
                                                 (0, leaf1) ->
                                                   InternalTransition(0, List(Char(y, leaf1), Prepend(x, y))))
    val callTransition = Map[(Int, Tree), CallTransition[Tree, Tree]]((0, openA) ->
                                                                        CallTransition(0, openA, Nil),
                                                                      (0, openB) ->
                                                                        CallTransition(0, openB, Nil))
    val returnTransition = Map[(Int, Tree, Tree), ReturnTransition[Tree]](
      (0, openA, closeA) ->
        ReturnTransition(0, List(Subtree(z, openA, closeA), SubstInY(x, z), Append(x, xp))),
      (0, openB, closeB) ->
        ReturnTransition(0, List(Subtree(z, openB, closeB), SubstInY(x, z), Append(x, xp)))
    )
    val finalStates = Map(0 -> Expr0.Var[Tree](x))
    val reverse =
      new STT[IO, Map, Tree, Tree, Tree](0,
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

  test("reverse tree (symbolic)") {

    import Assignment._
    val internalTransition: PartialFunction[(Int, Tree), InternalTransition[Tree]] = { case (0, l @ Tree.Leaf(_)) =>
      InternalTransition(0, List(Char(y, l), Prepend(x, y)))
    }
    val callTransition: PartialFunction[(Int, Tree), CallTransition[Tree, Tree]] = { case (0, o @ Tree.Open(_)) =>
      CallTransition(0, o, Nil)
    }
    val returnTransition: PartialFunction[(Int, Tree, Tree), ReturnTransition[Tree]] = {
      case (0, open @ Tree.Open(nopen), close @ Tree.Close(nclose)) if nopen == nclose =>
        ReturnTransition(0, List(Subtree(z, open, close), SubstInY(x, z), Append(x, xp)))
    }
    val finalStates = Map(0 -> Expr0.Var[Tree](x))
    val reverse =
      new STT[IO, PartialFunction, Tree, Tree, Tree](0,
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

  test("emit until error") {
    import Assignment._
    val internalTransition =
      Map[(Int, Tree), InternalTransition[Tree]]()
    val callTransition = Map[(Int, Tree), CallTransition[Tree, Unit]](
      (0, openA) ->
        CallTransition(0, (), List(Char(y, openA), Append(x, y))))
    val returnTransition = Map[(Int, Unit, Tree), ReturnTransition[Tree]](
      (0, (), closeA) ->
        ReturnTransition(0, List(Char(y, closeA), Append(x, y), Prepend(x, xp)))
    )
    val finalStates = Map(0 -> Expr0.Var[Tree](x))
    val onlyA = new STT[IO, Map, Tree, Tree, Unit](0,
                                                   internalTransition,
                                                   callTransition,
                                                   returnTransition,
                                                   finalStates,
                                                   Map("x" -> Type.Type0, "y" -> Type.Type0))
    Stream(openA, openA, closeA, closeA, openB, closeB)
      .through(onlyA)
      .attempt
      .compile
      .toList
      .map { result =>
        expect(
          result == List(Right(openA),
                         Right(openA),
                         Right(closeA),
                         Right(closeA),
                         Left(STTException("malformed input, prefix <b is not accepted"))))
      }
  }

}
