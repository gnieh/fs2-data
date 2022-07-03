package fs2
package data
package xml
package xpath

import weaver._

import cats.effect.IO
import cats.syntax.all._

object QueryPipeSpec extends SimpleIOSuite {

  test("simple query") {

    val query =
      XPath(List(Location(Axis.Descendent, QName("a").some, None), Location(Axis.Child, QName("c").some, None)))

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
      .through(new QueryPipe(query.pdfa))
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

  test("any element") {
    val query = XPath(List(Location(Axis.Descendent, QName("c").some, None), Location(Axis.Child, None, None)))

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
      .through(new QueryPipe(query.pdfa))
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

    val query = XPath(List(Location(Axis.Descendent, QName("a").some, Some(Predicate.Eq(QName("attr"), "value")))))

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
      .through(new QueryPipe(query.pdfa))
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

    val query = XPath(List(Location(Axis.Descendent, QName("a").some, Some(Predicate.Neq(QName("attr"), "value")))))

    Stream
      .emit("""<a>without</a>
              |<a attr="value">with value</a>
              |<a attr="other value">with other value</a>""".stripMargin)
      .covary[IO]
      .through(events())
      .through(new QueryPipe(query.pdfa))
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

}
