/*
 * Copyright 2022 Lucas Satabin
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

package fs2
package data.csv
package generic

import cats.Eq
import semiauto._
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.catsSyntaxEq
import weaver._

object CsvRowDecoderTest extends SimpleIOSuite {

  val csvRow = CsvRow.unsafe(NonEmptyList.of("1", "test", "42"), NonEmptyList.of("i", "s", "j"))
  val csvRowDefaultI = CsvRow.unsafe(NonEmptyList.of("", "test", "42"), NonEmptyList.of("i", "s", "j"))
  val csvRowNoI =
    CsvRow.unsafe(NonEmptyList.of("test", "42"), NonEmptyList.of("s", "j"))
  val csvRowEmptyJ =
    CsvRow.unsafe(NonEmptyList.of("1", "test", ""), NonEmptyList.of("i", "s", "j"))
  val csvRowNoJ =
    CsvRow.unsafe(NonEmptyList.of("1", "test"), NonEmptyList.of("i", "s"))
  val csvRowEmptyCell = CsvRow.unsafe(NonEmptyList.of("1", "", "42"), NonEmptyList.of("i", "s", "j"))

  case class Test(i: Int = 0, s: String, j: Option[Int])
  case class TestOrder(s: String, j: Int, i: Int = 8888888)
  case class TestRename(s: String, @CsvName("j") k: Int, i: Int)
  case class TestOptionRename(s: String, @CsvName("j") k: Option[Int], i: Int)
  case class TestOptionalString(i: Int, s: Option[String], j: Int)

  implicit val testDataCsvRowDecoder: CsvRowDecoder[TestData, String] = deriveCsvRowDecoder[TestData]
  implicit val testDataRowDecoder: RowDecoder[TestData] = deriveRowDecoder[TestData]

  val testDecoder = deriveCsvRowDecoder[Test]
  val testOrderDecoder = deriveCsvRowDecoder[TestOrder]
  val testRenameDecoder = deriveCsvRowDecoder[TestRename]
  val testOptionRenameDecoder = deriveCsvRowDecoder[TestOptionRename]
  val testOptionalStringDecoder = deriveCsvRowDecoder[TestOptionalString]

  pureTest("case classes should be decoded properly by header name and not position") {
    expect(testDecoder(csvRow) == Right(Test(1, "test", Some(42)))) and
      expect(testOrderDecoder(csvRow) == Right(TestOrder("test", 42, 1)))
  }

  /*pureTest("case classes should be handled properly with default value and empty cell") {
    expect(testDecoder(csvRowDefaultI) == Right(Test(0, "test", Some(42))))
  }*/

  /*pureTest("case classes should be handled properly with default value and missing column") {
    expect(testDecoder(csvRowNoI) == Right(Test(0, "test", Some(42))))
  }*/

  pureTest("case classes should be handled properly with optional value and empty cell") {
    expect(testDecoder(csvRowEmptyJ) == Right(Test(1, "test", None)))
  }

  pureTest("case classes should be handled properly with optional value and missing column") {
    expect(testDecoder(csvRowNoJ) == Right(Test(1, "test", None)))
  }

  pureTest("case classes should be decoded according to their field renames") {
    expect(testRenameDecoder(csvRow) == Right(TestRename("test", 42, 1)))
  }

  pureTest("case classes should be decoded according to their field renames if value is optional") {
    expect(testOptionRenameDecoder(csvRow) == Right(TestOptionRename("test", Some(42), 1))) and
      expect(testOptionRenameDecoder(csvRowNoJ) == Right(TestOptionRename("test", None, 1)))
  }

  pureTest("allow empty strings as string cell values") {
    expect(testDecoder(csvRowEmptyCell) == Right(Test(1, "", Some(42)))) and
      expect(testOptionalStringDecoder(csvRowEmptyCell) == Right(TestOptionalString(1, None, 42)))
  }

  implicit val decoderErrorEq: Eq[Throwable] = (a, b) => a.toString === b.toString

  implicit val decoderTestDataEq: Eq[TestData] = (a, b) => a == b

  test("Parser should return all decoder results as values when using attemptDecodeUsingHeaders") {
    val content =
      """name,age,description
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected: List[Either[Throwable, TestData]] = List(
      Right(TestData("John Doe", "47", "description 1")),
      Left(new CsvException("Headers have size 3 but row has size 2. Both numbers must match! in line 3", None)),
      Right(TestData("Bob Smith", "80", "description 2")),
      Left(new CsvException("Headers have size 3 but row has size 2. Both numbers must match! in line 5", None))
    )

    Stream
      .emit(content)
      .covary[IO]
      .through(attemptDecodeUsingHeaders[TestData](','))
      .compile
      .toList
      .map(actual => expect.eql(expected, actual))
  }

  test("Parser should return only errors as values when using attemptDecodeUsingHeaders with wrong header") {
    val content =
      """name1,age,description
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected: List[Either[Throwable, TestData]] = List(
      Left(new DecoderError("unknown column name 'name' in line 2", None)),
      Left(new CsvException("Headers have size 3 but row has size 2. Both numbers must match! in line 3", None)),
      Left(new DecoderError("unknown column name 'name' in line 4", None)),
      Left(new CsvException("Headers have size 3 but row has size 2. Both numbers must match! in line 5", None))
    )

    Stream
      .emit(content)
      .covary[IO]
      .through(attemptDecodeUsingHeaders[TestData](','))
      .compile
      .toList
      .map(actual => expect.eql(expected, actual))
  }

  test("Parser should return all decoder results as values when using attemptDecodeGivenHeaders") {
    val content =
      """name-should-not-be-used,age-should-not-be-used,description-should-not-be-used
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected: List[Either[Throwable, TestData]] = List(
      Right(TestData("John Doe", "47", "description 1")),
      Left(new CsvException("Headers have size 3 but row has size 2. Both numbers must match! in line 3", None)),
      Right(TestData("Bob Smith", "80", "description 2")),
      Left(new CsvException("Headers have size 3 but row has size 2. Both numbers must match! in line 5", None))
    )

    Stream
      .emit(content)
      .covary[IO]
      .through(
        attemptDecodeGivenHeaders[TestData](separator = ',',
                                            skipHeaders = true,
                                            headers = NonEmptyList.of("name", "age", "description")))
      .compile
      .toList
      .map(actual => expect.eql(expected, actual))
  }

  test("Parser should return all decoder results when using attemptDecodeGivenHeaders with skipHeaders = false") {
    val content =
      """John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected: List[Either[Throwable, TestData]] = List(
      Right(TestData("John Doe", "47", "description 1")),
      Left(new CsvException("Headers have size 3 but row has size 2. Both numbers must match! in line 2", None)),
      Right(TestData("Bob Smith", "80", "description 2")),
      Left(new CsvException("Headers have size 3 but row has size 2. Both numbers must match! in line 4", None))
    )

    Stream
      .emit(content)
      .covary[IO]
      .through(
        attemptDecodeGivenHeaders[TestData](separator = ',',
                                            skipHeaders = false,
                                            headers = NonEmptyList.of("name", "age", "description")))
      .compile
      .toList
      .map(actual => expect.eql(expected, actual))
  }

  test("Parser should return all decoder results when using attemptDecodeSkippingHeaders") {
    val content =
      """name-should-not-be-used,age-should-not-be-used,description-should-not-be-used
        |John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected: List[Either[Throwable, TestData]] = List(
      Right(TestData("John Doe", "47", "description 1")),
      Left(new DecoderError("unexpect end of row", None)),
      Right(TestData("Bob Smith", "80", "description 2")),
      Left(new DecoderError("unexpect end of row", None))
    )

    Stream
      .emit(content)
      .covary[IO]
      .through(attemptDecodeSkippingHeaders[TestData](separator = ','))
      .compile
      .toList
      .map(actual => expect.eql(expected, actual))
  }

  test("Parser should return all decoder results when using attemptDecodeWithoutHeaders") {
    val content =
      """John Doe,47,description 1
        |Jane Doe,50
        |Bob Smith,80,description 2
        |Alice Grey,78
        |""".stripMargin

    val expected: List[Either[Throwable, TestData]] = List(
      Right(TestData("John Doe", "47", "description 1")),
      Left(new DecoderError("unexpect end of row", None)),
      Right(TestData("Bob Smith", "80", "description 2")),
      Left(new DecoderError("unexpect end of row", None))
    )

    Stream
      .emit(content)
      .covary[IO]
      .through(attemptDecodeWithoutHeaders[TestData](separator = ','))
      .compile
      .toList
      .map(actual => expect.eql(expected, actual))
  }
}
