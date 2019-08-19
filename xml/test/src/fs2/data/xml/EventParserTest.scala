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
import fs2.io._

import org.scalatest.{events => _, _}

import scala.concurrent._

import better.files.{Resource => _, _}

import java.util.concurrent._

class EventParserTest extends FlatSpec with Matchers with EitherValues with BeforeAndAfterAll {

  private var executor: ExecutorService = _
  private var blocker: Blocker = _

  implicit val cs = IO.contextShift(ExecutionContext.global)

  override def beforeAll(): Unit = {
    executor = Executors.newFixedThreadPool(2)
    blocker = Blocker.liftExecutorService(executor)
  }

  override def afterAll(): Unit = {
    executor.shutdown()
  }

  // valid tests
  for (path <- File("xml/test/resources/xmlconf/xmltest/valid/").list(_.name.endsWith(".xml")) ++ File(
         "xml/test/resources/xmlconf/sun/valid/").list) {
    s"File ${path.toString}" should "be parsed correctly" in {
      file
        .readAll[IO](path.path, blocker, 1024)
        .through(fs2.text.utf8Decode)
        .flatMap(Stream.emits(_))
        .through(events)
        .compile
        .toList
        .unsafeRunSync()

    }
  }

}
