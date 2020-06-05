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

import io.circe.parser._
import fs2._
import fs2.io._
import cats.effect._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent._
import better.files.{Resource => _, _}
import java.util.concurrent._

class CsvParserTest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

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

  private val testFileDir: File = File("csv/test/resources/csv-spectrum/csvs/")
  for (path <- testFileDir.list) {
    val expected =
      parse(File(
        s"csv/test/resources/csv-spectrum/json/${path.nameWithoutExtension}.json").contentAsString)
        .flatMap(_.as[List[Map[String, String]]])
        .toTry
        .get
    s"File ${testFileDir.relativize(path).toString}" should "be parsed correctly" in {
      val actual =
        file
          .readAll[IO](path.path, blocker, 1024)
          .through(fs2.text.utf8Decode)
          .flatMap(Stream.emits(_))
          .through(rows[IO]())
          .through(headers[IO, String])
          .compile
          .toList
          .unsafeRunSync()
          .map(_.toMap)
      actual should be(expected)
    }

    it should "be encoded and parsed correctly" in {
      val reencoded =
        Stream
          .emits(expected)
          .map(m => CsvRow.fromListHeaders(m.toList))
          .unNone
          .through(encodeRowWithFirstHeaders)
          .through(toStrings())
          .flatMap(Stream.emits(_))
          .through(rows[IO]())
          .through(headers[IO, String])
          .compile
          .toList
          .unsafeRunSync()
          .map(_.toMap)

      reencoded should be(expected)
    }
  }
}
