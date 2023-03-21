package fs2
package data
package csv

import cats.syntax.all._
import weaver._

object RowParserTest extends SimpleIOSuite {

  pureTest("Line numbers account for empty lines (#461)") {
    val input =
      """A,B,C
        |D,E,F
        |
        |G,H,I
        |
        |J,K,L
        |""".stripMargin

    val result = Stream
      .emit(input)
      .covary[Fallible]
      .through(lowlevel.rows[Fallible, String]())
      .map(r => s"""${r.line.orEmpty}: ${r.values.toList.mkString}""")
      .compile
      .toList

    expect.same(Right(List("1: ABC", "2: DEF", "4: GHI", "6: JKL")), result)
  }

}
