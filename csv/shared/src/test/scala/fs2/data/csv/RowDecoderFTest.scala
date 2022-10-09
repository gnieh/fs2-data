package fs2
package data
package csv

import cats.effect.IO
import weaver._

object RowDecoderFTest extends SimpleIOSuite {

  case class TwoNumbers(a: Int, b: Int)

  implicit val rowDecoder: RowDecoder[TwoNumbers] = RowDecoder.instance { row =>
    for {
      a <- row.asAt[Int](0)
      b <- row.asAt[Int](1)
    } yield TwoNumbers(a, b)
  }

  implicit val csvRowDecoder: CsvRowDecoder[TwoNumbers, String] = CsvRowDecoder.instance { row =>
    for {
      a <- row.as[Int]("a")
      b <- row.as[Int]("b")
    } yield TwoNumbers(a, b)
  }

  test("can parse CSV with rows that do not convert by an attempting RowDecoder") {
    val rows = List(
      "4,5\n",
      "6,seven"
    )
    Stream
      .emits[IO, String](rows)
      .through(decodeWithoutHeaders[DecoderResult[TwoNumbers]]())
      .compile
      .toList
      .map { result =>
        expect(result.head.isRight) and expect(result.tail.head.isLeft)
      }
  }

  test("can parse CSV with rows that do not convert by an attempting CsvRowDecoder") {
    val rows = List(
      "a,b\n",
      "4,5\n",
      "6,seven"
    )
    Stream
      .emits[IO, String](rows)
      .through(decodeUsingHeaders[DecoderResult[TwoNumbers]]())
      .compile
      .toList
      .map { result =>
        expect(result.head.isRight) and expect(result.tail.head.isLeft)
      }
  }

}
