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
import weaver._

import scala.util.{Failure, Success, Try}

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

  test("decodeUsingHeaders should map data to case class matching the header name to the case class field name") {
    implicit val decoder: CsvRowDecoder[Test, String] = testDecoder
    implicit val testEq: Eq[Test] = (a, b) => a == b

    val content =
      """s,i,j
        |a,12,3
        |""".stripMargin

    val expected: List[Test] = List(
      Test(12, "a", Some(3))
    )

    Stream
      .emit(content)
      .covary[IO]
      .through(decodeUsingHeaders[Test](','))
      .compile
      .toList
      .map(actual => expect.eql(expected, actual))
  }

  test("decodeUsingHeaders should succeed if an optional field is missing") {
    implicit val decoder: CsvRowDecoder[Test, String] = testDecoder
    implicit val testEq: Eq[Test] = (a, b) => a == b

    val content =
      """s,i
        |a,12
        |""".stripMargin

    val expected: List[Test] = List(
      Test(12, "a", None)
    )

    Stream
      .emit(content)
      .covary[IO]
      .through(decodeUsingHeaders[Test](','))
      .compile
      .toList
      .map(actual => expect.eql(expected, actual))
  }

  test("decodeUsingHeaders should succeed if a required field with default value is missing") {
    implicit val decoder: CsvRowDecoder[Test, String] = testDecoder
    implicit val testEq: Eq[Test] = (a, b) => a == b

    val content =
      """s,j
        |a,3
        |""".stripMargin

    val expected: List[Test] = List(
      Test(0, "a", Some(3))
    )

    Stream
      .emit(content)
      .covary[IO]
      .through(decodeUsingHeaders[Test](','))
      .compile
      .toList
      .map(actual => expect.eql(expected, actual))

    // FIXME: This test succeeds on Scala 2.x but fails on Scala 3 with the error:
    // `DecoderError: unable to decode '' as an integer`, caused by a NumberFormatException on an empty String.
  }

  pureTest("decodeUsingHeaders should fail if a required string field is missing") {
    implicit val decoder: CsvRowDecoder[Test, String] = testDecoder

    val content =
      """i,j
        |12,3
        |""".stripMargin

    val stream = Stream
      .emit(content)
      .covary[IO]
      .through(decodeUsingHeaders[Test](','))
      .compile
      .toList

    import cats.effect.unsafe.implicits.global
    Try(stream.unsafeRunSync()) match {
      case Failure(exception) => expect(exception.getMessage == "unknown column name 's' in line 2")
      case Success(x)         => failure(s"Stream succeeded with value $x")
    }

    // FIXME: This test succeeds on Scala 2.x but fails on Scala 3. The Stream will finish successfully with an empty string
    // in Test.s.
  }

}
