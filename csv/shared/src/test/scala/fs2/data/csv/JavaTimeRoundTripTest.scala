/*
 * Copyright 2019-2022 Lucas Satabin
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

import cats.syntax.all._
import java.time._
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import weaver._

object JavaTimeRoundTripTest extends SimpleIOSuite {

  private def validateRoundTrip[T](value: T)(implicit enc: CellEncoder[T], dec: CellDecoder[T]) = {
    val encoded: String = enc.apply(value)
    val decoded: DecoderResult[T] = dec.apply(encoded)
    expect(encoded == value.toString) and
      expect(decoded == Right(value))
  }

  pureTest("it should round trip java time classes with default fmts") {
    val expectedZonedDateTime: ZonedDateTime = ZonedDateTime.of(2021, 3, 8, 13, 4, 29, 6, ZoneId.systemDefault())
    val expectedInstant: Instant = expectedZonedDateTime.toInstant
    val expectedPeriod: Period = Period.ofDays(10)
    val expectedLocalDate: LocalDate = expectedZonedDateTime.toLocalDate
    val expectedLocalDateTime: LocalDateTime = expectedZonedDateTime.toLocalDateTime
    val expectedLocalTime: LocalTime = expectedZonedDateTime.toLocalTime
    val expectedOffsetDateTime: OffsetDateTime = expectedZonedDateTime.toOffsetDateTime
    val expectedOffsetTime: OffsetTime = expectedOffsetDateTime.toOffsetTime
    val expectedDuration: Duration = Duration.of(30, temporal.ChronoUnit.DAYS)
    val expectedMonthDay: MonthDay = MonthDay.of(expectedZonedDateTime.getMonth, expectedZonedDateTime.getDayOfMonth)
    val expectedYear: Year = Year.of(expectedZonedDateTime.getYear)
    val expectedYearMonth: YearMonth = YearMonth.from(expectedZonedDateTime)

    validateRoundTrip[Instant](expectedInstant) and
      validateRoundTrip[Period](expectedPeriod) and
      validateRoundTrip[LocalDate](expectedLocalDate) and
      validateRoundTrip[LocalDateTime](expectedLocalDateTime) and
      validateRoundTrip[LocalTime](expectedLocalTime) and
      validateRoundTrip[OffsetDateTime](expectedOffsetDateTime) and
      validateRoundTrip[OffsetTime](expectedOffsetTime) and
      validateRoundTrip[ZonedDateTime](expectedZonedDateTime) and
      DayOfWeek.values().toList.map(validateRoundTrip[DayOfWeek](_)).combineAll and
      validateRoundTrip[Duration](expectedDuration) and
      Month.values().toList.map(validateRoundTrip[Month](_)).combineAll and
      validateRoundTrip[MonthDay](expectedMonthDay) and
      validateRoundTrip[Year](expectedYear) and
      validateRoundTrip[YearMonth](expectedYearMonth) and
      ZoneId.getAvailableZoneIds.asScala.map(ZoneId.of(_)).map(validateRoundTrip[ZoneId](_)).toList.combineAll and
      List(ZoneOffset.UTC, ZoneOffset.MAX, ZoneOffset.MAX).map(validateRoundTrip[ZoneOffset](_)).combineAll
  }

  pureTest("it should round trip with overridden formats") {
    val expectedZonedDateTime: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2021, 3, 8, 13, 4, 29), ZoneOffset.UTC)
    val expectedString: String = "3/8/2021 13:04:29+0000"
    val encoded: String = enc(expectedZonedDateTime)
    val decoded: DecoderResult[ZonedDateTime] = dec(encoded)
    expect(encoded == expectedString) and
      decoded.map { d =>
        expect(d.toLocalDateTime == expectedZonedDateTime.toLocalDateTime) and
          expect(d.getOffset == expectedZonedDateTime.getOffset)
      }.combineAll
  }

  def enc(zdt: ZonedDateTime): String = {
    implicit val zonedDateTimeEncodeFmt: DateTimeFormatter = DateTimeFormatter.ofPattern("M/d/yyyy HH:mm:ssZ")
    val encoded: String = CellEncoder[ZonedDateTime].apply(zdt)
    encoded
  }

  def dec(encoded: String): DecoderResult[ZonedDateTime] = {
    implicit val zonedDateTimeDecodeFmt: DateTimeFormatter = DateTimeFormatter.ofPattern("M/d/yyyy HH:mm:ssZ")
    val decoded: DecoderResult[ZonedDateTime] = CellDecoder[ZonedDateTime].apply(encoded)
    decoded
  }
}
