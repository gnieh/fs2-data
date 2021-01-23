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

import weaver._

object AutoDerivationTest extends SimpleIOSuite {

  val csvRow = CsvRow.unsafe[String](NonEmptyList.of("1", "test", "42"), NonEmptyList.of("i", "s", "j"))
  val plainRow = Row(NonEmptyList.of("1", "test", "42"))

  case class Test(i: Int, s: String, j: Int)

  pureTest("auto derivation for CsvRows should work properly for a simple case class (importing auto._)") {
    import auto._
    expect(CsvRowDecoder[Test, String].apply(csvRow) == Right(Test(1, "test", 42))) and
      expect(CsvRowEncoder[Test, String].apply(Test(1, "test", 42)) == csvRow)
  }

  pureTest("auto derivation for CsvRow should work properly for a simple case class (importing auto.csvrow._)") {
    import auto.csvrow._
    expect(CsvRowDecoder[Test, String].apply(csvRow) == Right(Test(1, "test", 42))) and
      expect(CsvRowEncoder[Test, String].apply(Test(1, "test", 42)) == csvRow)
  }

  pureTest("auto derivation for CsvRow should prefer custom decoders over derived ones") {
    import auto._
    implicit val customDe: CsvRowDecoder[Test, String] = _ => Right(Test(0, "", 0))
    implicit val customEn: CsvRowEncoder[Test, String] = _ => csvRow
    expect(CsvRowDecoder[Test, String].apply(csvRow) == Right(Test(0, "", 0))) and
      expect(CsvRowEncoder[Test, String].apply(Test(0, "", 0)) == csvRow)
  }

  pureTest("auto derivation for Rows should work properly for a simple case class (importing auto._)") {
    import auto._
    expect(RowDecoder[Test].apply(plainRow.values) == Right(Test(1, "test", 42))) and
      expect(RowEncoder[Test].apply(Test(1, "test", 42)) == plainRow.values)
  }

  pureTest("auto derivation for Rows should work properly for a simple case class (importing auto.csvrow._)") {
    import auto.row._
    expect(RowDecoder[Test].apply(plainRow.values) == Right(Test(1, "test", 42))) and
      expect(RowEncoder[Test].apply(Test(1, "test", 42)) == plainRow.values)
  }

  pureTest("auto derivation for Rows should prefer custom decoders over derived ones") {
    import auto._
    implicit val customDe: RowDecoder[Test] = _ => Right(Test(0, "", 0))
    implicit val customEn: RowEncoder[Test] = _ => plainRow.values
    expect(RowDecoder[Test].apply(plainRow.values) == Right(Test(0, "", 0))) and
      expect(RowEncoder[Test].apply(Test(0, "", 0)) == plainRow.values)
  }

  pureTest("auto derivation for coproduct cells should work out of the box for enum-style sealed traits") {
    import auto._

    expect(CellDecoder[Simple].apply("On") == Right(On)) and
      expect(CellDecoder[Simple].apply("Off") == Right(Off)) and
      expect(CellDecoder[Simple].apply("foo").isLeft) and
      expect(CellEncoder[Simple].apply(On) == "On") and
      expect(CellEncoder[Simple].apply(Off) == "Off")
  }

  pureTest(
    "auto derivation for coproduct cells should work out of the box for enum-style sealed traits (importing auto.cell._)") {
    import auto.cell._

    expect(CellDecoder[Simple].apply("On") == Right(On)) and
      expect(CellDecoder[Simple].apply("Off") == Right(Off)) and
      expect(CellDecoder[Simple].apply("foo").isLeft) and
      expect(CellEncoder[Simple].apply(On) == "On") and
      expect(CellEncoder[Simple].apply(Off) == "Off")
  }

}
