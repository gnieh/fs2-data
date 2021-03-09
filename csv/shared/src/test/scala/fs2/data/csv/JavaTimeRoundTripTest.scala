package fs2.data.csv

import java.time._
import java.time.format.DateTimeFormatter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, EitherValues}
import scala.jdk.CollectionConverters._

class JavaTimeRoundTripTest extends AnyFlatSpec with Matchers with EitherValues {
  private def validateRoundTrip[T](value: T)(implicit enc: CellEncoder[T], dec: CellDecoder[T]): Assertion = {
    val encoded: String = enc.apply(value)
    val decoded: DecoderResult[T] = dec.apply(encoded)
    encoded shouldBe value.toString
    decoded shouldBe Right(value)
  }

  it should "round trip java time classes with default fmts" in {
    val expectedZonedDateTime: ZonedDateTime = ZonedDateTime.of(2021, 3, 8, 13, 4, 29, 6, ZoneId.of("America/New_York"))
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

    validateRoundTrip[Instant](expectedInstant)
    validateRoundTrip[Period](expectedPeriod)
    validateRoundTrip[LocalDate](expectedLocalDate)
    validateRoundTrip[LocalDateTime](expectedLocalDateTime)
    validateRoundTrip[LocalTime](expectedLocalTime)
    validateRoundTrip[OffsetDateTime](expectedOffsetDateTime)
    validateRoundTrip[OffsetTime](expectedOffsetTime)
    validateRoundTrip[ZonedDateTime](expectedZonedDateTime)
    DayOfWeek.values().toList.map(validateRoundTrip[DayOfWeek](_))
    validateRoundTrip[Duration](expectedDuration)
    Month.values().toList.map(validateRoundTrip[Month](_))
    validateRoundTrip[MonthDay](expectedMonthDay)
    validateRoundTrip[Year](expectedYear)
    validateRoundTrip[YearMonth](expectedYearMonth)
    ZoneId.getAvailableZoneIds.asScala.map(ZoneId.of(_)).map(validateRoundTrip[ZoneId](_))
    List(ZoneOffset.UTC, ZoneOffset.MAX, ZoneOffset.MAX).map(validateRoundTrip[ZoneOffset](_))
  }

  it should "round trip with overridden formats" in {
    val expectedZonedDateTime: ZonedDateTime =
      ZonedDateTime.of(LocalDateTime.of(2021, 3, 8, 13, 4, 29), ZoneId.of("America/New_York"))
    val expectedString: String = "3/8/2021 13:04:29-0500"
    val encoded: String = enc(expectedZonedDateTime)
    val decoded: DecoderResult[ZonedDateTime] = dec(encoded)
    encoded shouldBe expectedString
    decoded.map { d =>
      d.toLocalDateTime shouldBe expectedZonedDateTime.toLocalDateTime
      d.getOffset shouldBe expectedZonedDateTime.getOffset
    }
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
