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

package fs2
package data
package mft

import esp.{ESP, Tag}
import pattern._

import weaver._
import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all._

case class NodeGuard(names: Set[String], positive: Boolean)
object NodeGuard {
  implicit object evaluator extends Evaluator[NodeGuard, Tag[String]] {

    override def eval(guard: NodeGuard, tree: ConstructorTree[Tag[String]]): Option[Tag[String]] =
      tree match {
        case ConstructorTree(Tag.Name(n), _) => (guard.names.contains(n) == guard.positive).guard[Option].as(Tag.Open)
        case ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(n), _))) =>
          (guard.names.contains(n) == guard.positive).guard[Option].as(Tag.Open)
        case _ => None
      }

  }
}

object ESPSpec extends IOSuite {

  val Main = 0
  val Rev = 1

  val mft: MFT[NodeGuard, String, String] = dsl { implicit builder =>
    val main = state(args = 0, initial = true)
    val rev = state(args = 1)

    main(anyNode.when(NodeGuard(Set("rev"), false))) ->
      copy(main(x1)) ~ main(x2)
    main(aNode("rev")) ->
      node("rev") {
        rev(x1, eps)
      } ~ main(x2)
    main(anyLeaf) ->
      copy ~ main(x1)

    rev(anyNode) ->
      rev(x2, copy(rev(x1, eps)) ~ y(0))
    rev(anyLeaf) ->
      rev(x1, copy ~ y(0))
    rev(epsilon) ->
      y(0)
  }

  type Res = ESP[IO, NodeGuard, String, String]

  override def sharedResource: Resource[IO, Res] = Resource.eval(mft.esp)

  test("reverse tree") { esp =>
    Stream
      .emits(List[MiniXML](
          // format: off
          MiniXML.Open("a"),
            MiniXML.Open("rev"),
              MiniXML.Open("b"),
                MiniXML.Open("c"),
                MiniXML.Close("c"),
                MiniXML.Text("text"),
                MiniXML.Open("b"),
                MiniXML.Close("b"),
              MiniXML.Close("b"),
              MiniXML.Open("c"),
                MiniXML.Text("text"),
                MiniXML.Open("b"),
                MiniXML.Close("b"),
              MiniXML.Close("c"),
            MiniXML.Close("rev"),
          MiniXML.Close("a"),
          MiniXML.Open("a"),
          MiniXML.Close("a")
          // format: on
        ))
      .covary[IO]
      .through(esp.pipe[MiniXML, MiniXML])
      .compile
      .toList
      .map { events =>
        expect.eql(
          List[MiniXML](
            // format: off
            MiniXML.Open("a"),
              MiniXML.Open("rev"),
                MiniXML.Open("c"),
                  MiniXML.Open("b"),
                  MiniXML.Close("b"),
                  MiniXML.Text("text"),
                MiniXML.Close("c"),
                MiniXML.Open("b"),
                  MiniXML.Open("b"),
                  MiniXML.Close("b"),
                  MiniXML.Text("text"),
                  MiniXML.Open("c"),
                  MiniXML.Close("c"),
                MiniXML.Close("b"),
              MiniXML.Close("rev"),
            MiniXML.Close("a"),
            MiniXML.Open("a"),
            MiniXML.Close("a")
            // format: on
          ),
          events
        )
      }
  }

}
