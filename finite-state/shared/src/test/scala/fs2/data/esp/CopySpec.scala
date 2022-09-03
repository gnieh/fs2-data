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
package esp

import mft._

import cats.effect._

import weaver._
import fs2.data.pattern.MiniXML

object CopySpec extends IOSuite {

  type Res = ESP[IO, String, String]

  override def sharedResource: Resource[IO, Res] = Resource.eval {

    val mft =
      new MFT[String, String](
        0,
        Map(
          0 -> Rules(
            Nil,
            List(
              EventSelector.AnyNode -> Rhs.Call(1, Forest.Self, Nil),
              EventSelector.AnyLeaf -> Rhs.Call(1, Forest.Self, Nil),
              EventSelector.Epsilon -> Rhs.Call(1, Forest.Self, Nil)
            )
          ),
          1 -> Rules(
            Nil,
            List(
              EventSelector.AnyNode -> Rhs.Concat(Rhs.CopyNode(Rhs.Call(1, Forest.First, Nil)),
                                                  Rhs.Call(1, Forest.Second, Nil)),
              EventSelector.AnyLeaf -> Rhs.Concat(Rhs.CopyLeaf, Rhs.Call(1, Forest.First, Nil)),
              EventSelector.Epsilon -> Rhs.Epsilon
            )
          )
        )
      )

    mft.esp
  }

  test("copy self") { esp =>
    val input = List(
      // format: off
      MiniXML.Open("a"),
        MiniXML.Text("Some text"),
      MiniXML.Close("a")
      // format: on
    )
    Stream
      .emits(input)
      .through(esp.pipe[MiniXML, MiniXML])
      .compile
      .toList
      .map { events =>
        expect.eql(input, events)
      }
  }

}
