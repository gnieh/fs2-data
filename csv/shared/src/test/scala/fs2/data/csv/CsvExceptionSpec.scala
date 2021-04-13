package fs2.data.csv

import cats.data.NonEmptyList
import fs2.{Fallible, Stream}

import weaver._

object CsvExceptionSpec extends SimpleIOSuite {

  pureTest("previous valid events should be emitted before Exception") {

    val input = """1,2,3
                  |a,b,c
                  |""a""".stripMargin

    val stream = Stream.emit(input).through(lowlevel.rows[Fallible, String]()).attempt

    expect(stream.compile.toList match {
      case Right(
            List(Right(Row(NonEmptyList("1", List("2", "3")))),
                 Right(Row(NonEmptyList("a", List("b", "c")))),
                 Left(e: CsvException))) =>
        e.line.contains(3) // check that we have the correct line number here
      case _ => false
    })

  }

}
