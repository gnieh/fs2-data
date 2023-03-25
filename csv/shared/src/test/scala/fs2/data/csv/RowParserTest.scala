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

package fs2
package data
package csv

import cats.syntax.all._
import weaver._

object RowParserTest extends SimpleIOSuite {

  pureTest("Line numbers account for empty lines (#461)") {
    val input =
      """A,B,C
        |D,E,F
        |
        |G,H,I
        |
        |J,K,L
        |""".stripMargin

    val result = Stream
      .emit(input)
      .covary[Fallible]
      .through(lowlevel.rows[Fallible, String]())
      .map(r => s"""${r.line.orEmpty}: ${r.values.toList.mkString}""")
      .compile
      .toList

    expect.same(Right(List("1: ABC", "2: DEF", "4: GHI", "6: JKL")), result)
  }

}
