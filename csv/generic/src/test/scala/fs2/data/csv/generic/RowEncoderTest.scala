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
package fs2.data.csv
package generic

import hlist._
import semiauto._

import cats.data.NonEmptyList
import shapeless._

import weaver._

object RowEncoderTest extends SimpleIOSuite {

  val csvRow = NonEmptyList.of("1", "test", "42")
  val csvRowEmptyI = NonEmptyList.of("", "test", "42")
  val csvRowEmptyJ = NonEmptyList.of("1", "test", "")

  case class Test(i: Int, s: String, j: Int)
  case class TestOrder(s: String, i: Int, j: Int)
  case class TestOptI(i: Option[Int], s: String, j: Int)
  case class TestOptJ(i: Int, s: String, j: Option[Int])

  val testEncoder = deriveRowEncoder[Test]
  val testOrderEncoder = deriveRowEncoder[TestOrder]
  val testOptIEncoder = deriveRowEncoder[TestOptI]
  val testOptJEncoder = deriveRowEncoder[TestOptJ]

  pureTest("case class should be handled with positional fields") {
    expect(testEncoder(Test(1, "test", 42)) == csvRow)
  }

  pureTest("case class should be handled properly with optional fields") {
    expect(testOptIEncoder(TestOptI(Some(1), "test", 42)) == csvRow) and
      expect(testOptIEncoder(TestOptI(None, "test", 42)) == csvRowEmptyI) and
      expect(testOptJEncoder(TestOptJ(1, "test", Some(42))) == csvRow) and
      expect(testOptJEncoder(TestOptJ(1, "test", None)) == csvRowEmptyJ)
  }

  pureTest("hlist should be handled properly") {
    expect(
      RowEncoder[Int :: String :: Int :: HNil]
        .apply(1 :: "test" :: 42 :: HNil) == csvRow)
  }

  pureTest("hlist should be handled properly with optional columns") {
    val encoder = RowEncoder[Option[Int] :: String :: Option[Int] :: HNil]

    expect(encoder(Some(1) :: "test" :: Some(42) :: HNil) == csvRow) and
      expect(encoder(None :: "test" :: Some(42) :: HNil) == csvRowEmptyI) and
      expect(encoder(Some(1) :: "test" :: None :: HNil) == csvRowEmptyJ)
  }

}
