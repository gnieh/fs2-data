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
package json

import cats.effect._
import fs2.data.json.internals.{BuilderChunkAccumulator, TokenChunkAccumulator}
import weaver._

import ast._
import io.file.{Files, Flags, Path}

sealed trait Expectation
object Expectation {
  case object Valid extends Expectation
  case object Invalid extends Expectation
  case object ImplementationDefined extends Expectation
}

abstract class JsonParserTest[Json](implicit builder: Builder[Json]) extends SimpleIOSuite {

  private val testFileDir = Path("json/src/test/resources/test-parsing/")

  private def standardTests(parsingPipe: Pipe[IO, String, Json]): IO[Expectations] =
    Files[IO]
      .list(testFileDir)
      .evalMap { path =>
        val expectation =
          if (path.fileName.toString.startsWith("y_"))
            Expectation.Valid
          else if (path.fileName.toString.startsWith("n_"))
            Expectation.Invalid
          else
            Expectation.ImplementationDefined

        val contentStream =
          Files[IO]
            .readAll(path, 1024, Flags.Read)
            .through(fs2.text.utf8.decode)

        contentStream
          .through(parsingPipe)
          .compile
          .onlyOrError
          .attempt
          .flatMap(actual =>
            expectation match {
              case Expectation.Valid | Expectation.ImplementationDefined =>
                contentStream.compile.string.map { rawExpected =>
                  val expected = parse(rawExpected)
                  expect(actual.isRight == expected.isRight) and (if (actual.isRight)
                                                                    expect(actual == expected)
                                                                  else success)
                }
              case Expectation.Invalid =>
                IO.pure(expect(actual.isLeft, path.toString))
            })
      }
      .compile
      .foldMonoid

  test("Standard test suite files should be parsed correctly") {
    standardTests(
      _.through(tokens)
        .through(ast.values))
  }

  test("Standard test suite files should be parsed directly correctly") {
    standardTests(_.through(ast.parse))
  }

  test("Standard test suite files should be parsed correctly with legacy parser") {
    standardTests(
      _.through(new json.internals.LegacyTokenParser(_).parse(new TokenChunkAccumulator).stream)
        .through(ast.values))
  }

  test("Standard test suite files should be parsed directly correctly with legacy parser") {
    standardTests(_.through(new json.internals.LegacyTokenParser(_).parse(new BuilderChunkAccumulator(builder)).stream))
  }

  def parse(content: String): Either[Throwable, Json]
}
