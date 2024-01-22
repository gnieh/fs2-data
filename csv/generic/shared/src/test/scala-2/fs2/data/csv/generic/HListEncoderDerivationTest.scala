/*
 * Copyright 2024 fs2-data Project
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
import cats.data.NonEmptyList
import shapeless._

import weaver._

object HListEncoderDerivationTest extends SimpleIOSuite {

  val csvRow = Row(NonEmptyList.of("1", "test", "42"))
  val csvRowEmptyI = Row(NonEmptyList.of("", "test", "42"))
  val csvRowEmptyJ = Row(NonEmptyList.of("1", "test", ""))

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
