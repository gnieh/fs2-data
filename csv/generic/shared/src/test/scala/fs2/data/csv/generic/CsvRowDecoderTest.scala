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
package fs2.data.csv
package generic

//import hlist._
import cats.data.NonEmptyList
import cats.syntax.all._
import fs2.data.csv.generic.semiauto._

import scala.{compiletime, deriving, quoted}
//import shapeless._

import weaver._

object CsvRowDecoderTest extends SimpleIOSuite {

  val csvRow = CsvRow.fromNelHeaders(NonEmptyList.of("renamed" -> "1", "s" -> "test", "j" -> "42"))

  case class Test(@CsvName("renamed") i: Int, s: String = "DEFAULT", j: Int)
  case class TestOrder(s: String, i: Int, j: Int)

  val testDecoder = deriveCsvRowDecoder[Test]
  val testOrderDecoder = deriveCsvRowDecoder[TestOrder]

  loggedTest("debug") { log =>
    log.info(Debug.debugSingle(deriveCsvRowDecoder[Test])).as(expect(false))
  }

  pureTest("case class should be handled with named fields") {
    expect(testDecoder(csvRow) == Right(Test(1, "test", 42)))
  }

}
