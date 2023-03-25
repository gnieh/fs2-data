/*
 * Copyright 2023 Lucas Satabin
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

import cats.effect._
import cats.syntax.all._
import fs2._
import fs2.data._
import weaver._

object LineNumberPreservationTest extends SimpleIOSuite {

  test("lowlevel.headers pipe preserves line numbers") {
    Stream("""h1,h2,h3
             |a,b,c
             |d,e,f
             |g,h,i
             |""".stripMargin)
      .covary[IO]
      .through(csv.lowlevel.rows())
      .through(csv.lowlevel.headers[IO, String])
      .zipWithIndex
      .map { case (row, idx) => expect.eql((idx + 2).some, row.line) } // idx +1 for headers, +1 as lines are 1-based
      .compile
      .foldMonoid
  }

}
