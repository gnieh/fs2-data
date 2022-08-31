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

package fs2.data
package pattern

import esp._
import matching.Table

import weaver._

import cats.effect._
import cats.Show
import cats.syntax.all._

sealed trait MiniXML
object MiniXML {
  case class Open(name: String) extends MiniXML
  case class Close(name: String) extends MiniXML
  case class Text(txt: String) extends MiniXML

  implicit object MiniXMLSelectable extends Selectable[MiniXML, Tag[Text]] {

    override def tree(e: MiniXML): ConstructorTree[Tag[Text]] =
      e match {
        case Open(name)  => ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(name), Nil)))
        case Close(name) => ConstructorTree(Tag.Close, List(ConstructorTree(Tag.Name(name), Nil)))
        case Text(t)     => ConstructorTree(Tag.Value(Text(t)), Nil)
      }

  }

}

object PatSpec extends IOSuite {

  type Res = DecisionTree[Tag[MiniXML.Text], Int]

  val compiler = new Compiler[IO, Tag[MiniXML.Text], Pattern[MiniXML.Text], Int](Pattern.heuristic)

  override def sharedResource: Resource[IO, Res] =
    Resource.eval {
      val cases = List[(Pattern[MiniXML.Text], Int)](
        Pattern.State(Some(0), Some(0), Pattern.Leaf(MiniXML.Text("one"))) -> 1,
        Pattern.State(Some(0), Some(1), Pattern.Leaf(MiniXML.Text("two"))) -> 2,
        Pattern.State(Some(0),
                      Some(0),
                      Pattern.Or(Pattern.Leaf(MiniXML.Text("two")), Pattern.Leaf(MiniXML.Text("three")))) -> 42,
        Pattern.State(Some(0), Some(1), Pattern.Wildcard) -> 20000,
        Pattern.Wildcard -> -1
      )

      compiler.compile(cases)
    }

  implicit val showText: Show[MiniXML.Text] = Show.show(_.txt)

  test("Pattern search 'one'") { tree =>
    IO(expect.same(Some(1), Table[Res, Input[MiniXML], Int].get(tree)(Input(0, 0, MiniXML.Text("one").some))))
  }

  test("Pattern search 'two'") { tree =>
    IO(expect.same(Some(42), Table[Res, Input[MiniXML], Int].get(tree)(Input(0, 0, MiniXML.Text("two").some))))
  }

  test("Pattern search 'two' depth 1") { tree =>
    IO(expect.same(Some(2), Table[Res, Input[MiniXML], Int].get(tree)(Input(0, 1, MiniXML.Text("two").some))))
  }

  test("Pattern search any") { tree =>
    IO(expect.same(Some(-1), Table[Res, Input[MiniXML], Int].get(tree)(Input(7, 2, MiniXML.Text("two").some))))
  }

  test("Pattern search anything") { tree =>
    IO(expect.same(Some(20000), Table[Res, Input[MiniXML], Int].get(tree)(Input(0, 1, MiniXML.Text("something").some))))
  }

}
