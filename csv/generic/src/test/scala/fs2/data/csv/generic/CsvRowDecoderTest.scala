/*
 * Copyright 2019 Lucas Satabin
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

import semiauto._
import cats.data.NonEmptyList

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
  val csvRowA = CsvRow.unsafe(NonEmptyList.of("7", "1", "test", "42"), NonEmptyList.of("a", "i", "s", "j"))
  val csvRowAOnly = CsvRow.unsafe(NonEmptyList.of("7"), NonEmptyList.of("a"))
  val csvRowAInvalidI = CsvRow.unsafe(NonEmptyList.of("7", "no-int", "test", "42"), NonEmptyList.of("a", "i", "s", "j"))

  case class Test(i: Int = 0, s: String, j: Option[Int])
  case class TestOrder(s: String, j: Int, i: Int)
  case class TestRename(s: String, @CsvName("j") k: Int, i: Int)
  case class TestOptionRename(s: String, @CsvName("j") k: Option[Int], i: Int)
  case class TestEmbed(a: Int, @CsvEmbed inner: Test)
  case class TestEmbedDefault(a: Int, @CsvEmbed inner: Test = Test(0, "", None))

  val testDecoder = deriveCsvRowDecoder[Test]
  val testOrderDecoder = deriveCsvRowDecoder[TestOrder]
  val testRenameDecoder = deriveCsvRowDecoder[TestRename]
  val testOptionRenameDecoder = deriveCsvRowDecoder[TestOptionRename]
  val testEmbedDecoder = {
    implicit val embedded: CsvRowDecoder[Test, String] = testDecoder
    deriveCsvRowDecoder[TestEmbed]
  }
  val testEmbedDefaultDecoder = {
    implicit val embedded: CsvRowDecoder[Test, String] = testDecoder
    deriveCsvRowDecoder[TestEmbedDefault]
  }

  pureTest("case classes should be decoded properly by header name and not position") {
    expect(testDecoder(csvRow) == Right(Test(1, "test", Some(42)))) and
      expect(testOrderDecoder(csvRow) == Right(TestOrder("test", 42, 1)))
  }

  pureTest("case classes should be handled properly with default value and empty cell") {
    expect(testDecoder(csvRowDefaultI) == Right(Test(0, "test", Some(42))))
  }

  pureTest("case classes should be handled properly with default value and missing column") {
    expect(testDecoder(csvRowNoI) == Right(Test(0, "test", Some(42))))
  }

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

  pureTest("case classes should be decoded respecting @CsvEmbed") {
    expect(testEmbedDecoder(csvRowA) == Right(TestEmbed(7, Test(1, "test", Some(42)))))
  }

  pureTest("case classes should be handled with defaults on @CsvEmbed if nested fields are missing") {
    expect(testEmbedDefaultDecoder(csvRowAOnly) == Right(TestEmbedDefault(7)))
  }

  pureTest("case classes should fail to decode on @CsvEmbed if nested fields are invalid") {
    expect(testEmbedDefaultDecoder(csvRowAInvalidI).isLeft)
  }

}
