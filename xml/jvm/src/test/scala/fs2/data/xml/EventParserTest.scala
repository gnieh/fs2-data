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
package fs2.data.xml

import cats.effect._

import fs2._
import fs2.io.file.{Files, Flags, Path}

import weaver._

object EventParserTest extends SimpleIOSuite {

  val testFileDir = Path("xml/jvm/src/test/resources/xmlconf")
  test("Standard test suite should pass") {
    (Files[IO].walk(testFileDir.resolve("xmltest/valid")).filter(_.fileName.endsWith(".xml")) ++
      Files[IO].list(testFileDir.resolve("sun/valid")))
      .evalMap { path =>
        // valid tests
        Files[IO]
          .readAll(path, 1024, Flags.Read)
          .through(fs2.text.utf8.decode)
          .flatMap(Stream.emits(_))
          .through(events)
          .compile
          .drain
          .attempt
          .map(res => expect(res.isRight, s"Failed to parse $path"))
      }
      .compile
      .foldMonoid
  }

}
