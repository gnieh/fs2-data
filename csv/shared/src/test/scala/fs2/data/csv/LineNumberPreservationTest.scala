package fs2.data.csv

import cats.effect._
import cats.syntax.all._
import fs2._
import fs2.data._
import weaver._

object LineNumberPreservationTest extends SimpleIOSuite {

  test("lowlevel.headers pipe preserves line numbers") {
    Stream("""h1,h2,h3
             |a,b,c
             |d,e,f
             |g,h,i
             |""".stripMargin)
      .covary[IO]
      .through(csv.lowlevel.rows())
      .through(csv.lowlevel.headers[IO, String])
      .zipWithIndex
      .map { case (row, idx) => expect.eql((idx + 2).some, row.line) } // idx +1 for headers, +1 as lines are 1-based
      .compile
      .foldMonoid
  }

}
