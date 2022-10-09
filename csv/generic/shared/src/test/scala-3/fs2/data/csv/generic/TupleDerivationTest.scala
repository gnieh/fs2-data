/*
 * Copyright 2022 Lucas Satabin
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

import tuples.{given}

import cats.data.NonEmptyList
import weaver._

object TupleDerivationTest extends SimpleIOSuite {

  val csvRow = Row(NonEmptyList.of("1", "test", "42"))
  val csvRowEmptyI = Row(NonEmptyList.of("", "test", "42"))
  val csvRowEmptyJ = Row(NonEmptyList.of("1", "test", ""))

  pureTest("tuple encoding should be handled properly") {
    val encoder = RowEncoder[(Int, String, Int)]
    expect(encoder.apply((1, "test", 42)) == csvRow)
  }

  pureTest("tuple encoding should be handled properly with optional columns") {
    val encoder = RowEncoder[(Option[Int], String, Option[Int])]

    expect(encoder((Some(1), "test", Some(42))) == csvRow) and
      expect(encoder((None, "test", Some(42))) == csvRowEmptyI) and
      expect(encoder((Some(1), "test", None)) == csvRowEmptyJ)
  }

  pureTest("tuple decoding should be handled properly") {
    val decoder = RowDecoder[(Int, String, Int)]
    expect(decoder.apply(csvRow) == Right((1, "test", 42)))
  }

  pureTest("tuple decoding should be handled properly with optional columns") {
    val decoder = RowDecoder[(Option[Int], String, Option[Int])]

    expect(decoder(csvRow) == Right((Some(1), "test", Some(42)))) and
      expect(decoder(csvRowEmptyI) == Right((None, "test", Some(42)))) and
      expect(decoder(csvRowEmptyJ) == Right((Some(1), "test", None)))
  }

}
