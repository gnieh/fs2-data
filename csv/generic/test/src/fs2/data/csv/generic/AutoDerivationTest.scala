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
import fs2.data.csv.{CsvRow, CsvRowDecoder, RowDecoder}
import org.scalatest._

class AutoDerivationTest extends FlatSpec with Matchers {

  val csvRow = new CsvRow(NonEmptyList.of("1", "test", "42"), Some(NonEmptyList.of("i", "s", "j")))
  val plainRow = CsvRow.fromNel(NonEmptyList.of("1", "test", "42"))

  case class Test(i: Int, s: String, j: Int)

  "auto derivation for CsvRowDecoder" should "work properly for a simple case class (importing auto._)" in {
    import auto._
    CsvRowDecoder[Test, String].apply(csvRow) shouldBe Right(Test(1, "test", 42))
  }

  it should "work properly for a simple case class (importing auto.csvrow._)" in {
    import auto.csvrow._
    CsvRowDecoder[Test, String].apply(csvRow) shouldBe Right(Test(1, "test", 42))
  }

  it should "prefer custom decoders over derived ones" in {
    import auto._
    implicit val custom: CsvRowDecoder[Test, String] = _ => Right(Test(0, "", 0))
    CsvRowDecoder[Test, String].apply(csvRow) shouldBe Right(Test(0, "", 0))
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
    import auto._
    implicit val custom: RowDecoder[Test] = _ => Right(Test(0, "", 0))
    RowDecoder[Test].apply(plainRow.values) shouldBe Right(Test(0, "", 0))
  }

}
