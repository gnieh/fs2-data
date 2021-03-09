package fs2.data.csv

import java.util.UUID

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class CellEncoderTest extends AnyFlatSpec with Matchers with EitherValues {

  "CellEncoder" should "have implicit instances available for standard types" in {
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
  }

  it should "decode standard types correctly" in {
    CellEncoder[Unit].apply(()) shouldBe ""
    CellEncoder[Int].apply(78) shouldBe "78"
    CellEncoder[Boolean].apply(true) shouldBe "true"
    CellEncoder[Char].apply('C') shouldBe "C"
    CellEncoder[Double].apply(1.2) shouldBe "1.2"
    CellEncoder[BigDecimal].apply(BigDecimal("12e455")) shouldBe "1.2E+456"
    CellEncoder[String].apply("foobar") shouldBe "foobar"
    CellEncoder[FiniteDuration].apply(2.seconds) shouldBe "2 seconds"

    CellEncoder[java.net.URI]
      .apply(new java.net.URI("http://localhost:8080/path?a=b")) shouldBe "http://localhost:8080/path?a=b"
    CellEncoder[java.util.UUID]
      .apply(UUID.fromString("6f55090e-a807-49c2-a142-2a0db1f079df")) shouldBe "6f55090e-a807-49c2-a142-2a0db1f079df"
    CellEncoder[java.time.LocalTime]
      .apply(java.time.LocalTime.of(13, 4, 29)) shouldBe "13:04:29"
  }

}
