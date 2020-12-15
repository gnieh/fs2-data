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
package fs2
package data
package json

import ast._
import io.file.Files

import cats.effect._

import weaver._
import java.nio.file.Paths

sealed trait Expectation
object Expectation {
  case object Valid extends Expectation
  case object Invalid extends Expectation
  case object ImplementationDefined extends Expectation
}

abstract class JsonParserTest[Json](implicit builder: Builder[Json]) extends SimpleIOSuite {

  private val testFileDir = Paths.get("json/jvm/src/test/resources/test-parsing/")

  test("Standard test suite files should be parsed correctly") {
    Files[IO]
      .directoryStream(testFileDir)
      .evalMap { path =>
        val expectation =
          if (path.toFile.getName.startsWith("y_"))
            Expectation.Valid
          else if (path.toFile.getName.startsWith("n_"))
            Expectation.Invalid
          else
            Expectation.ImplementationDefined

        val contentStream =
          Files[IO]
            .readAll(path, 1024)
            .through(fs2.text.utf8Decode)

        contentStream
          .through(tokens)
          .through(values)
          .compile
          .toList
          .flatMap(l =>
            if (l.size == 1) IO.pure(l.head) else IO.raiseError(new Exception("a single value is expected")))
          .attempt
          .flatMap(actual =>
            expectation match {
              case Expectation.Valid | Expectation.ImplementationDefined =>
                contentStream.compile.string.map { rawExpected =>
                  val expected = parse(rawExpected)
                  expect(actual.isRight == expected.isRight) and (if (actual.isRight) expect(actual == expected)
                                                                  else success)
                }
              case Expectation.Invalid =>
                IO.pure(expect(actual.isLeft == true))
            })
      }
      .compile
      .foldMonoid
  }

  def parse(content: String): Either[Throwable, Json]
}
