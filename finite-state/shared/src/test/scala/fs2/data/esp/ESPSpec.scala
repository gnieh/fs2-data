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

package fs2
package data
package mft

import esp.ESP
import pattern._

import weaver._
import cats.effect.IO
import cats.effect.Resource

object ESPSpec extends IOSuite {

  val Main = 0
  val Rev = 1

  val mft: MFT[String, String] =
    new MFT(
      Main,
      Map(
        Main -> Rules(
          Nil,
          List[(EventSelector[String], Rhs[String])](
            EventSelector.Node("rev") -> Rhs.Concat(Rhs.Node("rev", Rhs.Call(Rev, Forest.First, List(Rhs.Epsilon))),
                                                    Rhs.Call(Main, Forest.Second, Nil)),
            EventSelector.AnyNode -> Rhs.Concat(Rhs.CopyNode(Rhs.Call(Main, Forest.First, Nil)),
                                                Rhs.Call(Main, Forest.Second, Nil)),
            EventSelector.AnyLeaf -> Rhs.Concat(Rhs.CopyLeaf, Rhs.Call(Main, Forest.First, Nil))
          )
        ),
        Rev -> Rules(
          List(0),
          List(
            EventSelector.Node("a") -> Rhs.Call(
              Rev,
              Forest.Second,
              List(Rhs.Concat(Rhs.Node("a", Rhs.Call(Rev, Forest.First, List(Rhs.Epsilon))), Rhs.Param(0)))),
            EventSelector.Node("b") -> Rhs.Call(
              Rev,
              Forest.Second,
              List(Rhs.Concat(Rhs.Node("b", Rhs.Call(Rev, Forest.First, List(Rhs.Epsilon))), Rhs.Param(0)))),
            EventSelector.Node("c") -> Rhs.Call(
              Rev,
              Forest.Second,
              List(Rhs.Concat(Rhs.Node("c", Rhs.Call(Rev, Forest.First, List(Rhs.Epsilon))), Rhs.Param(0)))),
            EventSelector.Leaf("text") -> Rhs
              .Call(Rev, Forest.First, List(Rhs.Concat(Rhs.Leaf("text"), Rhs.Param(0)))),
            EventSelector.Epsilon -> Rhs.Param(0)
          )
        )
      )
    )

  type Res = ESP[IO, String, String]

  override def sharedResource: Resource[IO, Res] = Resource.eval(mft.esp)

  test("reverse tree") { esp =>
    Stream
      .emits(
        List[MiniXML](
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
