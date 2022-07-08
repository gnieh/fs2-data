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
package xml
package xpath

import literals._

import weaver._

import cats.effect.IO

object QueryPipeSpec extends SimpleIOSuite {

  test("simple query") {

    val query = xpath"//a/c"

    Stream
      .emit("""<a>
              |  <a>
              |    <c />
              |  </a>
              |  <b>
              |  </b>
              |  <c>2</c>
              |</a>""".stripMargin)
      .covary[IO]
      .through(events())
      .through(filter.raw(query))
      .parEvalMapUnbounded(_.compile.toList)
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(
            List(XmlEvent.StartTag(QName("c"), Nil, true), XmlEvent.EndTag(QName("c"))),
            List(XmlEvent.StartTag(QName("c"), Nil, false), XmlEvent.XmlString("2", false), XmlEvent.EndTag(QName("c")))
          ),
          tokens
        ))
  }

  test("simple query") {

    val query = xpath"//a/c"

    Stream
      .emit("""<a>
              |  <b>
              |    <c>
              |      <a>
              |        <c />
              |      </a>
              |    </c>
              |  </b>
              |</a>
              |<root><a><c>text</c></a></root>""".stripMargin)
      .covary[IO]
      .through(events())
      .through(filter.raw(query))
      .parEvalMapUnbounded(_.compile.toList)
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(
            List(XmlEvent.StartTag(QName("c"), Nil, true), XmlEvent.EndTag(QName("c"))),
            List(XmlEvent.StartTag(QName("c"), Nil, false),
                 XmlEvent.XmlString("text", false),
                 XmlEvent.EndTag(QName("c")))
          ),
          tokens
        ))
  }

  test("simple query early") {

    val query = xpath"//a"

    Stream
      .emit("""<a>
              |  <a>
              |    nested
              |  </a>
              |</a>""".stripMargin)
      .covary[IO]
      .through(events())
      .through(filter.collect(query, List, ordered = false))
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(
            List(XmlEvent.StartTag(QName("a"), Nil, false),
                 XmlEvent.XmlString("\n    nested\n  ", false),
                 XmlEvent.EndTag(QName("a"))),
            List(
              XmlEvent.StartTag(QName("a"), Nil, false),
              XmlEvent.XmlString("\n  ", false),
              XmlEvent.StartTag(QName("a"), Nil, false),
              XmlEvent.XmlString("\n    nested\n  ", false),
              XmlEvent.EndTag(QName("a")),
              XmlEvent.XmlString("\n", false),
              XmlEvent.EndTag(QName("a"))
            )
          ),
          tokens
        ))
  }

  test("any element") {
    val query = xpath"//c/*"

    Stream
      .emit("""<a>
              |  <b>
              |    <c>
              |      <a>
              |        <c />
              |      </a>
              |    </c>
              |  </b>
              |</a>
              |<root><a><c><a>text</a></c></a></root>""".stripMargin)
      .covary[IO]
      .through(events())
      .through(filter.raw(query))
      .parEvalMapUnbounded(_.compile.toList)
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(
            List(
              XmlEvent.StartTag(QName("a"), Nil, false),
              XmlEvent.XmlString("\n        ", false),
              XmlEvent.StartTag(QName("c"), Nil, true),
              XmlEvent.EndTag(QName("c")),
              XmlEvent.XmlString("\n      ", false),
              XmlEvent.EndTag(QName("a"))
            ),
            List(XmlEvent.StartTag(QName("a"), Nil, false),
                 XmlEvent.XmlString("text", false),
                 XmlEvent.EndTag(QName("a")))
          ),
          tokens
        ))
  }

  test("attribute query") {

    val query = xpath"""//a[@attr=="value"]"""

    Stream
      .emit("""<a>
              |  <b>
              |    <c>
              |      <a attr="value" />
              |      <a attr="yop">second</a>
              |    </c>
              |      <a attr="value">third
              |        <a attr="value">fourth</a>
              |      </a>
              |  </b>
              |</a>
              |<root><a><c>text</c></a></root>""".stripMargin)
      .covary[IO]
      .through(events())
      .through(filter.raw(query))
      .parEvalMapUnbounded(_.compile.toList)
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(
            List(
              XmlEvent.StartTag(QName("a"), List(Attr(QName("attr"), List(XmlEvent.XmlString("value", false)))), true),
              XmlEvent.EndTag(QName("a"))),
            List(
              XmlEvent.StartTag(QName("a"), List(Attr(QName("attr"), List(XmlEvent.XmlString("value", false)))), false),
              XmlEvent.XmlString("third\n        ", false),
              XmlEvent.StartTag(QName("a"), List(Attr(QName("attr"), List(XmlEvent.XmlString("value", false)))), false),
              XmlEvent.XmlString("fourth", false),
              XmlEvent.EndTag(QName("a")),
              XmlEvent.XmlString("\n      ", false),
              XmlEvent.EndTag(QName("a"))
            ),
            List(
              XmlEvent.StartTag(QName("a"), List(Attr(QName("attr"), List(XmlEvent.XmlString("value", false)))), false),
              XmlEvent.XmlString("fourth", false),
              XmlEvent.EndTag(QName("a"))
            )
          ),
          tokens
        ))
  }

  test("attribute neq query") {

    val query = xpath"""//a[@attr != "value"]"""

    Stream
      .emit("""<a>without</a>
              |<a attr="value">with value</a>
              |<a attr="other value">with other value</a>""".stripMargin)
      .covary[IO]
      .through(events())
      .through(filter.raw(query))
      .parEvalMapUnbounded(_.compile.toList)
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(
            List(
              XmlEvent.StartTag(QName("a"),
                                List(Attr(QName("attr"), List(XmlEvent.XmlString("other value", false)))),
                                false),
              XmlEvent.XmlString("with other value", false),
              XmlEvent.EndTag(QName("a"))
            )
          ),
          tokens
        ))
  }

  test("path disjunction") {
    val query = xpath"/a//c|//b"
    xml"""<a>
            <b>This is a b</b>
            <b>
              <c>This is a c in a b</c>
            </b>
          </a>"""
      .lift[IO]
      .through(filter.collect(query, List))
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(
            List(XmlEvent.StartTag(QName("b"), Nil, false),
                 XmlEvent.XmlString("This is a b", false),
                 XmlEvent.EndTag(QName("b"))),
            List(
              XmlEvent.StartTag(QName("b"), Nil, false),
              XmlEvent.XmlString("\n              ", false),
              XmlEvent.StartTag(QName("c"), Nil, false),
              XmlEvent.XmlString("This is a c in a b", false),
              XmlEvent.EndTag(QName("c")),
              XmlEvent.XmlString("\n            ", false),
              XmlEvent.EndTag(QName("b"))
            ),
            List(XmlEvent.StartTag(QName("c"), Nil, false),
                 XmlEvent.XmlString("This is a c in a b", false),
                 XmlEvent.EndTag(QName("c")))
          ),
          tokens
        ))
  }

}
