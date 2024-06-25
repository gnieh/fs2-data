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

import java.util.UUID

import weaver._

import scala.concurrent.duration._

import cats.Show

object CellEncoderTest extends SimpleIOSuite {

  // CellEncoder should have implicit instances available for standard types
  CellEncoder[String]
  CellEncoder[Array[Char]]
  CellEncoder[Boolean]
  CellEncoder[Unit]
  CellEncoder[Int]
  CellEncoder[BigDecimal]
  CellEncoder[FiniteDuration]
  CellEncoder[Duration]

  CellEncoder[java.net.URI]
  CellEncoder[java.util.UUID]
  CellDecoder[java.time.Instant]
  CellEncoder[java.time.Period]
  CellEncoder[java.time.LocalDate]
  CellEncoder[java.time.LocalDateTime]
  CellEncoder[java.time.LocalTime]
  CellEncoder[java.time.OffsetDateTime]
  CellEncoder[java.time.OffsetTime]
  CellEncoder[java.time.ZonedDateTime]
  CellEncoder[java.time.DayOfWeek]
  CellEncoder[java.time.Duration]
  CellEncoder[java.time.Month]
  CellEncoder[java.time.MonthDay]
  CellEncoder[java.time.Year]
  CellEncoder[java.time.YearMonth]
  CellEncoder[java.time.ZoneId]
  CellEncoder[java.time.ZoneOffset]

  pureTest("CellEncoder should encode standard types correctly") {
    expect(CellEncoder[Unit].apply(()) == "") and
      expect(CellEncoder[Int].apply(78) == "78") and
      expect(CellEncoder[Boolean].apply(true) == "true") and
      expect(CellEncoder[Char].apply('C') == "C") and
      expect(CellEncoder[Double].apply(1.2) == "1.2") and
      expect(CellEncoder[BigDecimal].apply(BigDecimal("12e455")) == "1.2E+456") and
      expect(CellEncoder[String].apply("foobar") == "foobar") and
      expect(CellEncoder[FiniteDuration].apply(2.seconds) == "2 seconds") and
      expect(
        CellEncoder[java.net.URI]
          .apply(new java.net.URI("http://localhost:8080/path?a=b")) == "http://localhost:8080/path?a=b") and
      expect(
        CellEncoder[java.util.UUID]
          .apply(UUID.fromString("6f55090e-a807-49c2-a142-2a0db1f079df")) == "6f55090e-a807-49c2-a142-2a0db1f079df") and
      expect(
        CellEncoder[java.time.LocalTime]
          .apply(java.time.LocalTime.of(13, 4, 29)) == "13:04:29")
  }

  pureTest("CellEncoder instance can be built from correct cats.Show instance") {
    implicit val showInt42: Show[Int] = Show.show(_ => "42")
    expect(CellEncoder.fromShow[Unit].apply(()) == "()") and
      expect(CellEncoder.fromShow[Int].apply(78) == "42")
  }

}
