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
package fs2.data.csv.generic

import cats.data.NonEmptyList
import fs2.data.csv.{CellDecoder, CellEncoder, CsvRow, CsvRowDecoder, CsvRowEncoder, Row, RowDecoder, RowEncoder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AutoDerivationTest extends AnyFlatSpec with Matchers {

  val csvRow = new CsvRow(NonEmptyList.of("1", "test", "42"),
                          NonEmptyList.of("i", "s", "j"))
  val plainRow = new Row(NonEmptyList.of("1", "test", "42"))

  case class Test(i: Int, s: String, j: Int)

  "auto derivation for CsvRows" should "work properly for a simple case class (importing auto._)" in {
    import auto._
    CsvRowDecoder[Test, String].apply(csvRow) shouldBe Right(Test(1, "test", 42))
    CsvRowEncoder[Test, String].apply(Test(1, "test", 42)) shouldBe csvRow
  }

  it should "work properly for a simple case class (importing auto.csvrow._)" in {
    import auto.csvrow._
    CsvRowDecoder[Test, String].apply(csvRow) shouldBe Right(Test(1, "test", 42))
    CsvRowEncoder[Test, String].apply(Test(1, "test", 42)) shouldBe csvRow
  }

  it should "prefer custom decoders over derived ones" in {
    import auto._
    implicit val customDe: CsvRowDecoder[Test, String] = _ => Right(Test(0, "", 0))
    implicit val customEn: CsvRowEncoder[Test, String] = _ => csvRow
    CsvRowDecoder[Test, String].apply(csvRow) shouldBe Right(Test(0, "", 0))
    CsvRowEncoder[Test, String].apply(Test(0, "", 0)) shouldBe csvRow
  }

  "auto derivation for Rows" should "work properly for a simple case class (importing auto._)" in {
    import auto._
    RowDecoder[Test].apply(plainRow.values) shouldBe Right(Test(1, "test", 42))
    RowEncoder[Test].apply(Test(1, "test", 42)) shouldBe plainRow.values
  }

  it should "work properly for a simple case class (importing auto.csvrow._)" in {
    import auto.row._
    RowDecoder[Test].apply(plainRow.values) shouldBe Right(Test(1, "test", 42))
    RowEncoder[Test].apply(Test(1, "test", 42)) shouldBe plainRow.values
  }

  it should "prefer custom decoders over derived ones" in {
    import auto._
    implicit val customDe: RowDecoder[Test] = _ => Right(Test(0, "", 0))
    implicit val customEn: RowEncoder[Test] = _ => plainRow.values
    RowDecoder[Test].apply(plainRow.values) shouldBe Right(Test(0, "", 0))
    RowEncoder[Test].apply(Test(0, "", 0)) shouldBe plainRow.values
  }

  "auto derivation for coproduct cells" should "work out of the box for enum-style sealed traits" in {
    import auto._

    CellDecoder[Simple].apply("On") shouldBe Right(On)
    CellDecoder[Simple].apply("Off") shouldBe Right(Off)
    CellDecoder[Simple].apply("foo").isLeft shouldBe true

    CellEncoder[Simple].apply(On) shouldBe "On"
    CellEncoder[Simple].apply(Off) shouldBe "Off"
  }

  "auto derivation for coproduct cells" should "work out of the box for enum-style sealed traits (importing auto.cell._)" in {
    import auto.cell._

    CellDecoder[Simple].apply("On") shouldBe Right(On)
    CellDecoder[Simple].apply("Off") shouldBe Right(Off)
    CellDecoder[Simple].apply("foo").isLeft shouldBe true

    CellEncoder[Simple].apply(On) shouldBe "On"
    CellEncoder[Simple].apply(Off) shouldBe "Off"
  }

}
