/*
 * Copyright 2022 Lucas Satabin
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
package xml
package scalaXml

import xpath._
import literals._

import weaver._

import cats.effect.IO
import scala.xml._

object XPathSpec extends SimpleIOSuite {

  test("DOM filtering") {
    val query = xpath"//a"

    val nested = Elem(null, "a", Null, TopScope, false, Text("\n    nested\n  "))
    Stream
      .emit("""<a>
              |  <a>
              |    nested
              |  </a>
              |</a>""".stripMargin)
      .covary[IO]
      .through(events())
      .through(filter.dom(query, deterministic = false))
      .compile
      .toList
      .map(nodes =>
        expect.same(Set(nested, Elem(null, "a", Null, TopScope, false, Text("\n  "), nested, Text("\n"))),
                    nodes.toSet[scala.xml.Node]))

  }

}
