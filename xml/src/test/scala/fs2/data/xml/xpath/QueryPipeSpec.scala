package fs2
package data
package xml
package xpath

import automaton._

import weaver._

import cats.effect.IO

object QueryPipeSpec extends SimpleIOSuite {

  test("simple query") {

    val query = XPath(List(Location(Axis.Descendent, QName("a")), Location(Axis.Child, QName("c"))))

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
      .through(new QueryPipe(query.dfa))
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

}
