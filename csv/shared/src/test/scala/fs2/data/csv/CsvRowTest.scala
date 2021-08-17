/*
 * Copyright 2021 Lucas Satabin
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

import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CsvRowTest extends AnyFlatSpec with Matchers {

  val csvRow = new CsvRow(NonEmptyList.of("1", "test", "42"), NonEmptyList.of("i", "s", "j"))

  "setting a value at a header" should "add the header if it is missing" in {
    val result = csvRow.set("k", "new")
    result.values shouldBe NonEmptyList.of("1", "test", "42", "new")
    result.headers shouldBe NonEmptyList.of("i", "s", "j", "k")
  }

  it should "replace the value if it exists" in {
    val result = csvRow.set("s", "new")
    result.values shouldBe NonEmptyList.of("1", "new", "42")
    result.headers shouldBe NonEmptyList.of("i", "s", "j")
  }

}
