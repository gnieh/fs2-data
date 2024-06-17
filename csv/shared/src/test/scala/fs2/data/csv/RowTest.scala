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

import weaver._

object RowTest extends SimpleIOSuite {

  pureTest("Row.asOptionalAt should return None for empty cells") {
    val row = new Row(List("", "2", "3"))
    expect(row.asOptionalAt[Int](0).contains(None))
  }

  pureTest("Row.asOptionalAt should return None for missing cells") {
    val row = new Row(List("", "2", "3"))
    expect(row.asOptionalAt[Int](3, missing = _ => Right(None)).contains(None))
  }

  pureTest("Row.asOptionalAt should return None for values considered empty") {
    val row = new Row(List("N/A", "2", "3"))
    expect(row.asOptionalAt[Int](0, isEmpty = _ == "N/A").contains(None))
  }

  pureTest("Row.asOptionalAt should return decoded value for non-empty cells") {
    val row = new Row(List("", "2", "3"))
    expect(row.asOptionalAt[Int](1).contains(Some(2)))
  }

  pureTest("Row.asOptionalAt should return None for empty cells") {
    val row = Row(List("", "2", "3"))
    expect(row.asOptionalAt[Int](0).contains(None))
  }

  pureTest("Row.asOptionalAt should return None for missing cells") {
    val row = Row(List("", "2", "3"))
    expect(row.asOptionalAt[Int](7, missing = _ => Right(None)).contains(None))
  }

  pureTest("Row.asOptionalAt should return None for values considered empty") {
    val row = Row(List("N/A", "2", "3"))
    expect(row.asOptionalAt[Int](0, isEmpty = _ == "N/A").contains(None))
  }

  pureTest("Row.asOptionalAt should return decoded value for non-empty cells") {
    val row = Row(List("", "2", "3"))
    expect(row.asOptionalAt[Int](1).contains(Some(2)))
  }
}
