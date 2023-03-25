/*
 * Copyright 2023 Lucas Satabin
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

package fs2.data.csv

import cats.Eq
import cats.data.NonEmptyList
import io.circe.parser.parse
import fs2._
import fs2.io.file.{Files, Flags, Path}
import fs2.data.text.utf8._
import cats.effect._
import cats.syntax.all._
import weaver._

object CsvParserTest extends SimpleIOSuite {

  private val testFileDir: Path = Path("csv/shared/src/test/resources/csv-spectrum/csvs/")

  case class Test(i: Int = 0, s: String, j: Option[Int])
  case class TestData(name: String, age: Int, description: String)

  implicit val testDataCsvRowDecoder: CsvRowDecoder[TestData, String] = (row: RowF[Some, String]) => {
    (
      row.as[String]("name"),
      row.as[Int]("age"),
      row.as[String]("description")
    ).mapN { case (name, age, description) =>
      TestData(name, age, description)
    }
  }

  implicit val testDataRowDecoder: RowDecoder[TestData] = new RowDecoder[TestData] {
    override def apply(row: RowF[NoneF, Nothing]): DecoderResult[TestData] = {
      (
        row.asAt[String](0),
        row.asAt[Int](1),
        row.asAt[String](2)
      ).mapN { case (name, age, description) =>
        TestData(name, age, description)
      }
    }
  }

  implicit val decoderErrorEq: Eq[CsvException] = Eq.by(_.toString)
  implicit val decoderTestDataEq: Eq[TestData] = Eq.fromUniversalEquals

  lazy val allExpected: Stream[IO, (Path, List[Map[String, String]])] =
    Files[IO]
      .list(testFileDir)
      .evalMap { path =>
        val name = path.fileName.toString.stripSuffix(".csv")
        Files[IO]
          .readAll(Path(s"csv/shared/src/test/resources/csv-spectrum/json/$name.json"), 1024, Flags.Read)
          .through(text.utf8.decode)
          .compile
          .string
          .flatMap { rawExpected =>
            parse(rawExpected)
              .flatMap(_.as[List[Map[String, String]]])
              .liftTo[IO]
          }
          .tupleLeft(path)
      }

  loggedTest("Standard test suite should pass") { log =>
    allExpected
      .evalTap { case (path, _) => log.info(path.fileName.toString) }
      .evalMap { case (path, expected) =>
        Files[IO]
          .readAll(path, 1024, Flags.Read)
          .through(decodeUsingHeaders[CsvRow[String]]())
          .compile
          .toList
          .map(_.map(_.toMap))
          .map(actual => expect.eql(expected, actual))
      }
      .compile
      .foldMonoid
  }

  loggedTest("Standard test suite files should be encoded and parsed correctly") { log =>
    allExpected
      .evalTap { case (path, _) => log.info(path.fileName.toString) }
      .evalMap { case (_, expected) =>
        Stream
          .emits(expected)
          .map(m => CsvRow.fromListHeaders(m.toList))
          .unNone
          .through[IO, String](encodeUsingFirstHeaders[CsvRow[String]]())
          .covary[IO]
          .through(decodeUsingHeaders[CsvRow[String]]())
          .compile
          .toList
          .map(_.map(_.toMap))
          .map(reencoded => expect.eql(expected, reencoded))
      }
      .compile
      .foldMonoid
  }

  test("Parser should handle literal quotes if specified") {
    val content =
      """name,age,description
        |John Doe,47,no quotes
        |Jane Doe,50,"entirely quoted"
        |Bob Smith,80,"starts with" a quote
        |Alice Grey,78,contains "a quote
        |""".stripMargin

    val expected = List(
      Map("name" -> "John Doe", "age" -> "47", "description" -> "no quotes"),
      Map("name" -> "Jane Doe", "age" -> "50", "description" -> "\"entirely quoted\""),
      Map("name" -> "Bob Smith", "age" -> "80", "description" -> "\"starts with\" a quote"),
      Map("name" -> "Alice Grey", "age" -> "78", "description" -> "contains \"a quote")
    )

    Stream
      .emit(content)
      .covary[IO]
      .through(decodeUsingHeaders[CsvRow[String]](',', QuoteHandling.Literal))
      .compile
      .toList
      .map(_.map(_.toMap))
      .map(actual => expect.eql(expected, actual))
  }

  pureTest("Parser should return all decoder results as values when using attemptDecodeUsingHeaders") {
    val content =
      """name,age,description
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Right(TestData("John Doe", 47, "description 1")),
      Left(new HeaderSizeError(3, 2, Some(3L))),
      Right(TestData("Bob Smith", 80, "description 2")),
      Left(new HeaderSizeError(3, 2, Some(5L)))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(lenient.attemptDecodeUsingHeaders[TestData](','))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("Parser should return only errors as values when using attemptDecodeUsingHeaders with wrong header") {
    val content =
      """name1,age,description
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Left(new DecoderError("unknown field name", None)),
      Left(new HeaderSizeError(3, 2, Some(3L))),
      Left(new DecoderError("unknown field name", None)),
      Left(new HeaderSizeError(3, 2, Some(5L)))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(lenient.attemptDecodeUsingHeaders[TestData](','))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("Parser should return all decoder results as values when using attemptDecodeGivenHeaders") {
    val content =
      """name-should-not-be-used,age-should-not-be-used,description-should-not-be-used
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Right(TestData("John Doe", 47, "description 1")),
      Left(new HeaderSizeError(3, 2, Some(3L))),
      Right(TestData("Bob Smith", 80, "description 2")),
      Left(new HeaderSizeError(3, 2, Some(5L)))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(
        lenient.attemptDecodeGivenHeaders[TestData](separator = ',',
                                                    skipHeaders = true,
                                                    headers = NonEmptyList.of("name", "age", "description")))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("Parser should return all decoder results when using attemptDecodeGivenHeaders with skipHeaders = false") {
    val content =
      """John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Right(TestData("John Doe", 47, "description 1")),
      Left(new HeaderSizeError(3, 2, Some(2L))),
      Right(TestData("Bob Smith", 80, "description 2")),
      Left(new HeaderSizeError(3, 2, Some(4L)))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(
        lenient.attemptDecodeGivenHeaders[TestData](separator = ',',
                                                    skipHeaders = false,
                                                    headers = NonEmptyList.of("name", "age", "description")))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("Parser should return all decoder results when using attemptDecodeSkippingHeaders") {
    val content =
      """name-should-not-be-used,age-should-not-be-used,description-should-not-be-used
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Right(TestData("John Doe", 47, "description 1")),
      Left(new DecoderError("unknown index 2", None)),
      Right(TestData("Bob Smith", 80, "description 2")),
      Left(new DecoderError("unknown index 2", None))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(lenient.attemptDecodeSkippingHeaders[TestData](separator = ','))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("Parser should return all decoder results when using attemptDecodeWithoutHeaders") {
    val content =
      """John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Right(TestData("John Doe", 47, "description 1")),
      Left(new DecoderError("unknown index 2", None)),
      Right(TestData("Bob Smith", 80, "description 2")),
      Left(new DecoderError("unknown index 2", None))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(lenient.attemptDecodeWithoutHeaders[TestData](separator = ','))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("should fail if a required string field is missing") {
    val row = CsvRow
      .unsafe(
        NonEmptyList.of("12", "3"),
        NonEmptyList.of("i", "j")
      )
      .withLine(Some(2))

    testDataCsvRowDecoder(row) match {
      case Left(error) => expect.eql(error.getMessage, "unknown field name")
      case Right(x)    => failure(s"Stream succeeded with value $x")
    }
  }

  pureTest("Parser should return all decoder results as values when using attemptDecodeUsingHeaders") {
    val content =
      """name,age,description
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Right(TestData("John Doe", 47, "description 1")),
      Left(new HeaderSizeError(3, 2, Some(3L))),
      Right(TestData("Bob Smith", 80, "description 2")),
      Left(new HeaderSizeError(3, 2, Some(5L)))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(lenient.attemptDecodeUsingHeaders[TestData](','))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("Parser should return only errors as values when using attemptDecodeUsingHeaders with wrong header") {
    val content =
      """name1,age,description
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Left(new DecoderError("unknown field name", None)),
      Left(new HeaderSizeError(3, 2, Some(3L))),
      Left(new DecoderError("unknown field name", None)),
      Left(new HeaderSizeError(3, 2, Some(5L)))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(lenient.attemptDecodeUsingHeaders[TestData](','))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("Parser should return all decoder results as values when using attemptDecodeGivenHeaders") {
    val content =
      """name-should-not-be-used,age-should-not-be-used,description-should-not-be-used
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Right(TestData("John Doe", 47, "description 1")),
      Left(new HeaderSizeError(3, 2, Some(3L))),
      Right(TestData("Bob Smith", 80, "description 2")),
      Left(new HeaderSizeError(3, 2, Some(5L)))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(
        lenient.attemptDecodeGivenHeaders[TestData](separator = ',',
                                                    skipHeaders = true,
                                                    headers = NonEmptyList.of("name", "age", "description")))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("Parser should return all decoder results when using attemptDecodeGivenHeaders with skipHeaders = false") {
    val content =
      """John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Right(TestData("John Doe", 47, "description 1")),
      Left(new HeaderSizeError(3, 2, Some(2L))),
      Right(TestData("Bob Smith", 80, "description 2")),
      Left(new HeaderSizeError(3, 2, Some(4L)))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(
        lenient.attemptDecodeGivenHeaders[TestData](separator = ',',
                                                    skipHeaders = false,
                                                    headers = NonEmptyList.of("name", "age", "description")))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("Parser should return all decoder results when using attemptDecodeSkippingHeaders") {
    val content =
      """name-should-not-be-used,age-should-not-be-used,description-should-not-be-used
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Right(TestData("John Doe", 47, "description 1")),
      Left(new DecoderError("unknown index 2", None)),
      Right(TestData("Bob Smith", 80, "description 2")),
      Left(new DecoderError("unknown index 2", None))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(lenient.attemptDecodeSkippingHeaders[TestData](separator = ','))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }

  pureTest("Parser should return all decoder results when using attemptDecodeWithoutHeaders") {
    val content =
      """John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected = List(
      Right(TestData("John Doe", 47, "description 1")),
      Left(new DecoderError("unknown index 2", None)),
      Right(TestData("Bob Smith", 80, "description 2")),
      Left(new DecoderError("unknown index 2", None))
    )

    val stream = Stream
      .emit(content)
      .covary[Fallible]
      .through(lenient.attemptDecodeWithoutHeaders[TestData](separator = ','))
      .compile
      .toList

    stream match {
      case Right(actual) => expect.eql(expected, actual)
      case Left(x)       => failure(s"Stream failed with value $x")
    }
  }
}
