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

object DupSpec extends IOSuite {

  type Res = ESP[IO, String, String]

  override def sharedResource: Resource[IO, Res] = Resource.eval {

    val mft = dsl[String, String] { implicit builder =>
      val init = state(0, initial = true)
      val dup = state(1)
      val cop = state(0)

      init(any) -> dup(x0, eps)

      dup(anyNode) -> copy(cop(x1)) ~ dup(x2, y(0) ~ copy(cop(x1)))
      dup(anyLeaf) -> copy ~ dup(x1, y(0) ~ copy)
      dup(epsilon) -> y(0)

      cop(anyNode) -> copy(cop(x1)) ~ cop(x2)
      cop(anyLeaf) -> copy ~ cop(x1)
      cop(epsilon) -> eps

    }

    mft.esp
  }

  test("duplicate") { esp =>
    val input = List(
      // format: off
      MiniXML.Open("a"),
        MiniXML.Open("b"),
          MiniXML.Text("Some text"),
        MiniXML.Close("b"),
        MiniXML.Text("Some other text"),
        MiniXML.Open("b"),
          MiniXML.Open("b"),
            MiniXML.Text("Some more text"),
          MiniXML.Close("b"),
        MiniXML.Close("b"),
        MiniXML.Text("And finally, some text"),
      MiniXML.Close("a")
      // format: on
    )
    Stream
      .emits(input)
      .through(esp.pipe[MiniXML, MiniXML])
      .compile
      .toList
      .map { events =>
        expect.eql(input ++ input, events)
      }
  }

}
