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

import weaver._

import scala.concurrent.duration._

object CellDecoderTest extends SimpleIOSuite {

  // CellDecoder should have implicit instances available for standard types
  CellDecoder[String]
  CellDecoder[Array[Char]]
  CellDecoder[Boolean]
  CellDecoder[Unit]
  CellDecoder[Int]
  CellDecoder[BigDecimal]
  CellDecoder[FiniteDuration]
  CellDecoder[Duration]

  CellDecoder[java.net.URI]
  CellDecoder[java.util.UUID]
  CellDecoder[java.time.Instant]
  CellDecoder[java.time.LocalTime]
  CellDecoder[java.time.ZonedDateTime]
  CellDecoder[java.time.Period]
  CellDecoder[java.time.LocalDate]
  CellDecoder[java.time.LocalDateTime]
  CellDecoder[java.time.LocalTime]
  CellDecoder[java.time.OffsetDateTime]
  CellDecoder[java.time.OffsetTime]
  CellDecoder[java.time.ZonedDateTime]
  CellDecoder[java.time.DayOfWeek]
  CellDecoder[java.time.Duration]
  CellDecoder[java.time.Month]
  CellDecoder[java.time.MonthDay]
  CellDecoder[java.time.Year]
  CellDecoder[java.time.YearMonth]
  CellDecoder[java.time.ZoneId]
  CellDecoder[java.time.ZoneOffset]

  CellDecoder[DecoderResult[Char]]
  CellDecoder[Either[String, Char]]

  pureTest("CellDecoder should decode standard types correctly") {
    expect(CellDecoder[Unit].apply("") == Right(())) and
      expect(CellDecoder[Int].apply("78") == Right(78)) and
      expect(CellDecoder[Boolean].apply("true") == Right(true)) and
      expect(CellDecoder[Char].apply("C") == Right('C')) and
      expect(CellDecoder[Double].apply("1.2") == Right(1.2)) and
      expect(CellDecoder[BigDecimal].apply("1.2e456") == Right(BigDecimal("12e455"))) and
      expect(CellDecoder[String].apply("foobar") == Right("foobar")) and
      expect(CellDecoder[FiniteDuration].apply("2 seconds") == Right(2.seconds)) and
      expect(CellDecoder[java.net.URI].apply("http://localhost:8080/path?a=b").isRight == true) and
      expect(
        CellDecoder[java.util.UUID].apply("6f55090e-a807-49c2-a142-2a0db1f079df").map(_.toString) == Right(
          "6f55090e-a807-49c2-a142-2a0db1f079df")) and
      expect(CellDecoder[java.time.LocalTime].apply("13:04:29") == Right(java.time.LocalTime.of(13, 4, 29)))
  }

  pureTest("CellDecoder should handle container types properly") {
    expect(CellDecoder[DecoderResult[Char]].apply("G") == Right(Right('G'))) and
      expect(CellDecoder[DecoderResult[Char]].apply("").map(_.isLeft) == Right(true)) and
      expect(CellDecoder[Either[String, Char]].apply("F") == Right(Right('F'))) and
      expect(CellDecoder[Either[String, Char]].apply("hello") == Right(Left("hello")))
  }

  pureTest("CellDecoder should fail on invalid inputs") {
    expect(CellDecoder[Unit].apply("some random non empty string").isLeft == true) and
      expect(CellDecoder[Int].apply("asdf").isLeft == true) and
      expect(CellDecoder[Boolean].apply("maybe").isLeft == true) and
      expect(CellDecoder[Char].apply("Chars").isLeft == true) and
      expect(CellDecoder[Double].apply("-").isLeft == true) and
      expect(CellDecoder[FiniteDuration].apply("2 meters").isLeft == true)
  }

}
