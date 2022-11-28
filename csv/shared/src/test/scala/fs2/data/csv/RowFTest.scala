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

import weaver._
import cats.data.NonEmptyList

object RowFTest extends SimpleIOSuite {

  pureTest("RowF.set should change the value at existing header") {
    val row = CsvRow.unsafe(NonEmptyList.of("1", "2", "3"), NonEmptyList.of("a", "b", "c"))
    expect.eql(NonEmptyList.of("1", "4", "3"), row.set("b", "4").values)
  }

  pureTest("RowF.set should add the value at end of row in case of missing header") {
    val row = CsvRow.unsafe(NonEmptyList.of("1", "2", "3"), NonEmptyList.of("a", "b", "c"))
    val extended = row.set("d", "4")
    expect.eql(NonEmptyList.of("1", "2", "3", "4"), extended.values) and
      expect.eql(NonEmptyList.of("a", "b", "c", "d"), extended.optHeaders.get)
  }

  pureTest("CsvRow.asNonEmpty should return None for empty cells") {
    val row = CsvRow.unsafe(NonEmptyList.of("", "2", "3"), NonEmptyList.of("a", "b", "c"))
    expect(row.asNonEmpty[Int]("a").contains(None))
  }

  pureTest("CsvRow.asNonEmpty should return decoded value for non-empty cells") {
    val row = CsvRow.unsafe(NonEmptyList.of("", "2", "3"), NonEmptyList.of("a", "b", "c"))
    expect(row.asNonEmpty[Int]("b").contains(Some(2)))
  }

  pureTest("Row.asNonEmptyAt should return None for empty cells") {
    val row = Row(NonEmptyList.of("", "2", "3"))
    expect(row.asNonEmptyAt[Int](0).contains(None))
  }

  pureTest("Row.asNonEmptyAt should return decoded value for non-empty cells") {
    val row = Row(NonEmptyList.of("", "2", "3"))
    expect(row.asNonEmptyAt[Int](1).contains(Some(2)))
  }
}
