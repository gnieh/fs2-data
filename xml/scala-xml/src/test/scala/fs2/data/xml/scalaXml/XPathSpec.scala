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
