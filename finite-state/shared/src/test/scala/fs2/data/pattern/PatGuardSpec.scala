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

package fs2.data
package pattern

import esp._

import weaver._

import cats.effect._
import cats.Show
import cats.syntax.all._

object PatGuardSpec extends IOSuite {

  implicit object evaluator extends Evaluator[Set[String], Tag[String]] {
    def eval(guard: Set[String], tree: ConstructorTree[Tag[String]]): Option[Tag[String]] =
      tree match {
        case ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(n), _))) if guard.contains(n) => Some(Tag.Open)
        case ConstructorTree(Tag.Name(n), _) if guard.contains(n)                                  => Some(Tag.Open)
        case _                                                                                     => None
      }
  }

  type Res = DecisionTree[Set[String], Tag[String], Int]

  val compiler = new Compiler[IO, Set[String], Tag[String], Pattern[Set[String], String], Int]

  override def sharedResource: Resource[IO, Res] =
    Resource.eval {
      val dsl = new PatternDsl[Set[String], String]
      import dsl._
      val cases = List(
        state(0, 0)(open when Set("a", "b")) -> 1,
        state(0, 0)(open when Set("d", "e")) -> 2,
        any -> 0
      )

      compiler.compile(cases)
    }

  implicit val showText: Show[MiniXML.Text] = Show.show(_.txt)

  test("Guard 'a'") { tree =>
    IO(expect.same(Some(1), tree.get(Input(0, 0, MiniXML.open("a").some))))
  }

  test("Guard 'b'") { tree =>
    IO(expect.same(Some(1), tree.get(Input(0, 0, MiniXML.open("b").some))))
  }

  test("Guard 'c'") { tree =>
    IO(expect.same(Some(0), tree.get(Input(0, 0, MiniXML.open("c").some))))
  }

  test("Guard 'd'") { tree =>
    IO(expect.same(Some(2), tree.get(Input(0, 0, MiniXML.open("d").some))))
  }

  test("Guard 'e'") { tree =>
    IO(expect.same(Some(2), tree.get(Input(0, 0, MiniXML.open("e").some))))
  }

  test("Guard 'f'") { tree =>
    IO(expect.same(Some(0), tree.get(Input(0, 0, MiniXML.open("f").some))))
  }

  test("Guard other state") { tree =>
    IO(expect.same(Some(0), tree.get(Input(4, 0, MiniXML.open("a").some))))
  }

  test("Guard other depth") { tree =>
    IO(expect.same(Some(0), tree.get(Input(0, 2, MiniXML.open("a").some))))
  }

}
