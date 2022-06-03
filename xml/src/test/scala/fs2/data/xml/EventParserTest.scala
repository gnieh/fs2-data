/*
 * Copyright 2019 Lucas Satabin
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
package fs2.data.xml

import cats.effect._

import fs2._
import fs2.io.file.{Files, Flags, Path}

import weaver._

object EventParserTest extends SimpleIOSuite {

  test("XML should generate proper events") {
    val input = """<root>
                  |  <a attr="value">
                  |    <b>Text</b>
                  |  </a>
                  |</root>""".stripMargin

    Stream
      .emits(input)
      .covary[IO]
      .through(events())
      .compile
      .toList
      .map(events =>
        expect(
          events == List(
            XmlEvent.StartDocument,
            XmlEvent.StartTag(QName("root"), Nil, false),
            XmlEvent.XmlString("\n  ", false),
            XmlEvent.StartTag(QName("a"), List(Attr(QName("attr"), List(XmlEvent.XmlString("value", false)))), false),
            XmlEvent.XmlString("\n    ", false),
            XmlEvent.StartTag(QName("b"), Nil, false),
            XmlEvent.XmlString("Text", false),
            XmlEvent.EndTag(QName("b")),
            XmlEvent.XmlString("\n  ", false),
            XmlEvent.EndTag(QName("a")),
            XmlEvent.XmlString("\n", false),
            XmlEvent.EndTag(QName("root")),
            XmlEvent.EndDocument
          )))
  }

  test("Empty XML document emits no events") {
    Stream
      .emits("")
      .covary[IO]
      .through(events())
      .compile
      .toList
      .map(events => expect(events.isEmpty))
  }

  test("Start and end document events should be properly emitted when several roots are in the input stream") {
    val input = "<a></a><a></a><a></a>"
    Stream
      .emits(input)
      .covary[IO]
      .through(events())
      .compile
      .toList
      .map(events =>
        expect(
          events == List(
            XmlEvent.StartDocument,
            XmlEvent.StartTag(QName("a"), Nil, false),
            XmlEvent.EndTag(QName("a")),
            XmlEvent.EndDocument,
            XmlEvent.StartDocument,
            XmlEvent.StartTag(QName("a"), Nil, false),
            XmlEvent.EndTag(QName("a")),
            XmlEvent.EndDocument,
            XmlEvent.StartDocument,
            XmlEvent.StartTag(QName("a"), Nil, false),
            XmlEvent.EndTag(QName("a")),
            XmlEvent.EndDocument
          )))
  }

  test("Comments should be ignored by default") {
    val input = """<!-- some comment -->
                  |<a>
                  |<!-- and another one -->
                  |Text
                  |</a>""".stripMargin
    Stream
      .emits(input)
      .covary[IO]
      .through(events())
      .compile
      .toList
      .map(events =>
        expect(
          events == List(
            XmlEvent.StartDocument,
            XmlEvent.StartTag(QName("a"), Nil, false),
            XmlEvent.XmlString("\n", false),
            XmlEvent.XmlString("\nText\n", false),
            XmlEvent.EndTag(QName("a")),
            XmlEvent.EndDocument
          )))
  }

  test("Comments should be emitted if asked for") {
    val input = """<!-- some comment -->
                  |<a>
                  |<!-- and another one -->
                  |Text
                  |</a>""".stripMargin
    Stream
      .emits(input)
      .covary[IO]
      .through(events(includeComments = true))
      .compile
      .toList
      .map(events =>
        expect(
          events == List(
            XmlEvent.StartDocument,
            XmlEvent.Comment(" some comment "),
            XmlEvent.StartTag(QName("a"), Nil, false),
            XmlEvent.XmlString("\n", false),
            XmlEvent.Comment(" and another one "),
            XmlEvent.XmlString("\nText\n", false),
            XmlEvent.EndTag(QName("a")),
            XmlEvent.EndDocument
          )))
  }

  val testFileDir = Path("xml/src/test/resources/xmlconf")
  test("Standard test suite should pass") {
    (Files[IO].walk(testFileDir.resolve("xmltest/valid")).filter(_.extName == ".xml") ++
      Files[IO].list(testFileDir.resolve("sun/valid")))
      .evalMap { path =>
        // valid tests
        Files[IO]
          .readAll(path, 1024, Flags.Read)
          .through(fs2.text.utf8.decode)
          .flatMap(Stream.emits(_))
          .through(events())
          .compile
          .drain
          .attempt
          .map(res => expect(res.isRight, s"Failed to parse $path"))
      }
      .compile
      .foldMonoid
  }

}
