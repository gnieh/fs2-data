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

import org.scalatest._

import cats.data.NonEmptyList

class CsvRowDecoderTest extends FlatSpec with Matchers {

  val csvRow = new CsvRow(NonEmptyList.of("1", "test", "42"), Some(NonEmptyList.of("i", "s", "j")))
  val csvRowDefaultI = new CsvRow(NonEmptyList.of("", "test", "42"), Some(NonEmptyList.of("i", "s", "j")))
  val csvRowNoI = new CsvRow(NonEmptyList.of("test", "42"), Some(NonEmptyList.of("s", "j")))
  val csvRowEmptyJ = new CsvRow(NonEmptyList.of("1", "test", ""), Some(NonEmptyList.of("i", "s", "j")))
  val csvRowNoJ = new CsvRow(NonEmptyList.of("1", "test"), Some(NonEmptyList.of("i", "s")))

  case class Test(i: Int = 0, s: String, j: Option[Int])
  case class TestOrder(s: String, j: Int, i: Int)

  val testDecoder = deriveCsvRowDecoder[Test]
  val testOrderDecoder = deriveCsvRowDecoder[TestOrder]

  "case classes" should "be decoded properly by header name and not position" in {
    testDecoder(csvRow) shouldBe Right(Test(1, "test", Some(42)))

    testOrderDecoder(csvRow) shouldBe Right(TestOrder("test", 42, 1))
  }

  it should "be handled properly with default value and empty cell" in {
    testDecoder(csvRowDefaultI) shouldBe Right(Test(0, "test", Some(42)))
  }

  it should "be handled properly with default value and missing column" in {
    testDecoder(csvRowNoI) shouldBe Right(Test(0, "test", Some(42)))
  }

  it should "be handled properly with optional value and empty cell" in {
    testDecoder(csvRowEmptyJ) shouldBe Right(Test(1, "test", None))
  }

  it should "be handled properly with optional value and missing column" in {
    testDecoder(csvRowNoJ) shouldBe Right(Test(1, "test", None))
  }

}
