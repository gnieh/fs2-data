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
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless._

class RowEncoderTest extends AnyFlatSpec with Matchers {

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

  "case class" should "be handled with positional fields" in {
    testEncoder(Test(1, "test", 42)) shouldBe csvRow
  }

  it should "be handled properly with optional fields" in {
    testOptIEncoder(TestOptI(Some(1), "test", 42)) shouldBe csvRow

    testOptIEncoder(TestOptI(None, "test", 42)) shouldBe csvRowEmptyI

    testOptJEncoder(TestOptJ(1, "test", Some(42))) shouldBe csvRow

    testOptJEncoder(TestOptJ(1, "test", None)) shouldBe csvRowEmptyJ
  }

  "hlist" should "be handled properly" in {
    RowEncoder[Int :: String :: Int :: HNil]
      .apply(1 :: "test" :: 42 :: HNil) shouldBe csvRow
  }

  it should "be handled properly with optional columns" in {
    val encoder = RowEncoder[Option[Int] :: String :: Option[Int] :: HNil]

    encoder(Some(1) :: "test" :: Some(42) :: HNil) shouldBe csvRow

    encoder(None :: "test" :: Some(42) :: HNil) shouldBe csvRowEmptyI

    encoder(Some(1) :: "test" :: None :: HNil) shouldBe csvRowEmptyJ
  }

}
