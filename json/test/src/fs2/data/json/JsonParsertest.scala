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
package fs2.data.json

import circe._

import io.circe.parser._

import fs2._
import fs2.io._

import cats.effect._

import org.scalatest._

import scala.concurrent._

import better.files.{Resource => _, _}

import java.util.concurrent._

sealed trait Expectation
object Expectation {
  case object Valid extends Expectation
  case object Invalid extends Expectation
  case object ImplementationDefined extends Expectation
}
class JsonParserTest extends FlatSpec with Matchers with BeforeAndAfterAll {

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

  private val testFileDir: File = File("json/test/resources/test-parsing/")
  for (path <- testFileDir.list) {
    s"File ${testFileDir.relativize(path).toString}" should "be parsed correctly" in {
      val expectation =
        if (path.name.startsWith("y_"))
          Expectation.Valid
        else if (path.name.startsWith("n_"))
          Expectation.Invalid
        else
          Expectation.ImplementationDefined

      val actual =
        file
          .readAll[IO](path.path, blocker, 1024)
          .through(fs2.text.utf8Decode)
          .flatMap(Stream.emits(_))
          .through(tokens)
          .through(values)
          .compile
          .toList
          .map(l => if (l.size == 1) l.head else throw new Exception("a single value is expected"))
          .attempt
          .unsafeRunSync()

      expectation match {
        case Expectation.Valid | Expectation.ImplementationDefined =>
          val expected =
            parse(path.contentAsString)
          actual.isRight shouldBe expected.isRight
          if (actual.isRight)
            actual shouldBe expected
        case Expectation.Invalid =>
          actual.isLeft shouldBe true
      }
    }
  }
}
