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

import fs2.{Fallible, Stream}

import weaver._

object CsvExceptionSpec extends SimpleIOSuite {

  pureTest("previous valid events should be emitted before Exception") {

    val input = """1,2,3
                  |a,b,c
                  |""a""".stripMargin

    val stream = Stream.emit(input).through(lowlevel.rows[Fallible, String]()).attempt

    expect(stream.compile.toList match {
      case Right(
            List(Right(Row(List("1", "2", "3"))),
                 Right(Row(List("a", "b", "c"))),
                 Left(e: CsvException))) =>
        e.line.contains(3L) // check that we have the correct line number here
      case _ => false
    })

  }

}
