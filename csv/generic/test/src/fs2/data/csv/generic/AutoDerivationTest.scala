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
import fs2.data.csv.{CellDecoder, CsvNelRowDecoder, CsvRow, RowDecoder}
import org.scalatest._

class AutoDerivationTest extends FlatSpec with Matchers {

  val csvRow = new CsvRow(NonEmptyList.of("1", "test", "42"), Some(NonEmptyList.of("i", "s", "j")))
  val plainRow = CsvRow.fromNel(NonEmptyList.of("1", "test", "42"))

  case class Test(i: Int, s: String, j: Int)

  sealed trait Simple
  case object On extends Simple
  case object Off extends Simple

  "auto derivation for CsvRowDecoder" should "work properly for a simple case class (importing auto._)" in {
    import auto._
    CsvNelRowDecoder[Test, String].apply(csvRow) shouldBe Right(Test(1, "test", 42))
  }

  it should "work properly for a simple case class (importing auto.csvrow._)" in {
    import auto.csvrow._
    CsvNelRowDecoder[Test, String].apply(csvRow) shouldBe Right(Test(1, "test", 42))
  }

  it should "prefer custom decoders over derived ones" in {
    implicit val custom: CsvNelRowDecoder[Test, String] = _ => Right(Test(0, "", 0))
    CsvNelRowDecoder[Test, String].apply(csvRow) shouldBe Right(Test(0, "", 0))
  }

  "auto derivation for RowDecoder" should "work properly for a simple case class (importing auto._)" in {
    import auto._
    RowDecoder[Test].apply(plainRow.values) shouldBe Right(Test(1, "test", 42))
  }

  it should "work properly for a simple case class (importing auto.csvrow._)" in {
    import auto.row._
    RowDecoder[Test].apply(plainRow.values) shouldBe Right(Test(1, "test", 42))
  }

  it should "prefer custom decoders over derived ones" in {
    implicit val custom: RowDecoder[Test] = _ => Right(Test(0, "", 0))
    RowDecoder[Test].apply(plainRow.values) shouldBe Right(Test(0, "", 0))
  }

  "auto derivation for coproduct cells" should "work out of the box for enum-style sealed traits" in {
    import auto._

    CellDecoder[Simple].apply("On") shouldBe Right(On)
    CellDecoder[Simple].apply("Off") shouldBe Right(Off)
    CellDecoder[Simple].apply("foo").isLeft shouldBe true
  }

  "auto derivation for coproduct cells" should "work out of the box for enum-style sealed traits (importing auto.cell._)" in {
    import auto.cell._

    CellDecoder[Simple].apply("On") shouldBe Right(On)
    CellDecoder[Simple].apply("Off") shouldBe Right(Off)
    CellDecoder[Simple].apply("foo").isLeft shouldBe true
  }

}
