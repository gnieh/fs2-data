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

  test("XML parser should handle all kind of XML elements") {
    val input = Stream.emits("""<?xml version="1.1" encoding="utf-8"?>
                               |<a att1="value1" att2="&amp; another one">
                               |  <!-- a comment -->
                               |  <b><![CDATA[Test]]></b>
                               |  <b/>
                               |</a>
                               |<?target content?>
                               |<!-- closing comment -->""".stripMargin)

    input
      .covary[IO]
      .through(events(true))
      .compile
      .toList
      .map(events =>
        expect(
          events == List(
            XmlEvent.StartDocument,
            XmlEvent.XmlDecl("1.1", Some("utf-8"), None),
            XmlEvent.StartTag(
              QName("a"),
              List(
                Attr(QName("att1"), List(XmlEvent.XmlString("value1", false))),
                Attr(QName("att2"),
                     List(XmlEvent.XmlString("", false),
                          XmlEvent.XmlEntityRef("amp"),
                          XmlEvent.XmlString(" another one", false)))
              ),
              false
            ),
            XmlEvent.XmlString("\n  ", false),
            XmlEvent.Comment(" a comment "),
            XmlEvent.XmlString("\n  ", false),
            XmlEvent.StartTag(QName("b"), Nil, false),
            XmlEvent.XmlString("Test", true),
            XmlEvent.EndTag(QName("b")),
            XmlEvent.XmlString("\n  ", false),
            XmlEvent.StartTag(QName("b"), Nil, true),
            XmlEvent.EndTag(QName("b")),
            XmlEvent.XmlString("\n", false),
            XmlEvent.EndTag(QName("a")),
            XmlEvent.XmlPI("target", "content"),
            XmlEvent.Comment(" closing comment "),
            XmlEvent.EndDocument
          )))
  }

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

  test("Postlog has precedence over prolog when several documents are concatenated") {
    val input = """<a></a><?target content?><?xml version="1.1"?><a></a><!-- comment --><a></a>"""
    Stream
      .emits(input)
      .covary[IO]
      .through(events(true))
      .compile
      .toList
      .map(events =>
        expect(
          events == List(
            XmlEvent.StartDocument,
            XmlEvent.StartTag(QName("a"), Nil, false),
            XmlEvent.EndTag(QName("a")),
            XmlEvent.XmlPI("target", "content"),
            XmlEvent.EndDocument,
            XmlEvent.StartDocument,
            XmlEvent.XmlDecl("1.1", None, None),
            XmlEvent.StartTag(QName("a"), Nil, false),
            XmlEvent.EndTag(QName("a")),
            XmlEvent.Comment(" comment "),
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

  test("An error should occur in case of early stream termination") {
    Stream
      .emits("<a>content")
      .through(events[IO, Char]())
      .attempt
      .compile
      .toList
      .map { evts =>
        expect(
          evts == List(
            Right(XmlEvent.StartDocument),
            Right(XmlEvent.StartTag(QName("a"), Nil, false)),
            Right(XmlEvent.XmlString("content", false)),
            Left(new XmlException(XmlSyntax("1"), "unexpected end of input"))
          ))
      }
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
          .attempt
          .compile
          .toList
          .map(expectWellFormed(_))
      }
      .compile
      .foldMonoid
  }

  def expectWellFormed(res: List[Either[Throwable, XmlEvent]]): Expectations =
    expect(res.isEmpty) ||
      expect(res.head == Right(XmlEvent.StartDocument)) &&
      expect(res.last == Right(XmlEvent.EndDocument))

}
