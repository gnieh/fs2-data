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
import hlist._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless._
import cats.data.NonEmptyList

class RowDecoderTest extends AnyFlatSpec with Matchers {

  val csvRow = NonEmptyList.of("1", "test", "42")
  val csvRowEmptyI = NonEmptyList.of("", "test", "42")
  val csvRowEmptyJ = NonEmptyList.of("1", "test", "")

  case class Test(i: Int, s: String, j: Int)
  case class TestOrder(s: String, i: Int, j: Int)
  case class TestOptI(i: Option[Int], s: String, j: Int)
  case class TestOptJ(i: Int, s: String, j: Option[Int])

  val testDecoder = deriveRowDecoder[Test]
  val testOrderDecoder = deriveRowDecoder[TestOrder]
  val testOptIDecoder = deriveRowDecoder[TestOptI]
  val testOptJDecoder = deriveRowDecoder[TestOptJ]

  "case class" should "be handled with positional fields" in {
    val res = testDecoder(csvRow)
    res shouldBe Right(Test(1, "test", 42))

    val fail = testOrderDecoder(csvRow)
    fail shouldBe Symbol("left")
  }

  it should "be handled properly with optional fields" in {
    val resi = testOptIDecoder(csvRow)
    resi shouldBe Right(TestOptI(Some(1), "test", 42))

    val resinone = testOptIDecoder(csvRowEmptyI)
    resinone shouldBe Right(TestOptI(None, "test", 42))

    val resj = testOptJDecoder(csvRow)
    resj shouldBe Right(TestOptJ(1, "test", Some(42)))

    val resjnone = testOptJDecoder(csvRowEmptyJ)
    resjnone shouldBe Right(TestOptJ(1, "test", None))
  }

  "hlist" should "be handled properly" in {
    RowDecoder[Int :: String :: Int :: HNil].apply(csvRow) shouldBe Right(
      1 :: "test" :: 42 :: HNil)
  }

  it should "be handled properly with optional columns" in {
    val decoder = RowDecoder[Option[Int] :: String :: Option[Int] :: HNil]

    decoder(csvRow) shouldBe Right(Some(1) :: "test" :: Some(42) :: HNil)

    decoder(csvRowEmptyI) shouldBe Right(None :: "test" :: Some(42) :: HNil)

    decoder(csvRowEmptyJ) shouldBe Right(Some(1) :: "test" :: None :: HNil)
  }

}
