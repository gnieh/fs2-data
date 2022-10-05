/*
 * Copyright 2019-2022 Lucas Satabin
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

object RowDecoderTest extends SimpleIOSuite {

  val csvRow = Row(NonEmptyList.of("1", "test", "42"))
  val csvRowEmptyI = Row(NonEmptyList.of("", "test", "42"))
  val csvRowEmptyJ = Row(NonEmptyList.of("1", "test", ""))

  case class Test(i: Int, s: String, j: Int)
  case class TestOrder(s: String, i: Int, j: Int)
  case class TestOptI(i: Option[Int], s: String, j: Int)
  case class TestOptJ(i: Int, s: String, j: Option[Int])

  val testDecoder = deriveRowDecoder[Test]
  val testOrderDecoder = deriveRowDecoder[TestOrder]
  val testOptIDecoder = deriveRowDecoder[TestOptI]
  val testOptJDecoder = deriveRowDecoder[TestOptJ]

  pureTest("case class should be handled with positional fields") {
    expect(testDecoder(csvRow) == Right(Test(1, "test", 42))) and
      expect(testOrderDecoder(csvRow).isLeft)
  }

  pureTest("case class should be handled properly with optional fields") {
    expect(testOptIDecoder(csvRow) == Right(TestOptI(Some(1), "test", 42))) and
      expect(testOptIDecoder(csvRowEmptyI) == Right(TestOptI(None, "test", 42))) and
      expect(testOptJDecoder(csvRow) == Right(TestOptJ(1, "test", Some(42)))) and
      expect(testOptJDecoder(csvRowEmptyJ) == Right(TestOptJ(1, "test", None)))
  }

}
