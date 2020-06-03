package fs2.data.csv

import java.util.UUID

import org.scalatest.{EitherValues, FlatSpec, Matchers}

import scala.concurrent.duration._

class CellEncoderTest extends FlatSpec with Matchers with EitherValues {

  "CellEncoder" should "have implicit instances available for standard types" in {
    CellEncoder[String]
    CellEncoder[Boolean]
    CellEncoder[Unit]
    CellEncoder[Int]
    CellEncoder[BigDecimal]
    CellEncoder[FiniteDuration]
    CellEncoder[Duration]

    CellEncoder[java.net.URL]
    CellEncoder[java.util.UUID]
    CellEncoder[java.time.Instant]
    CellEncoder[java.time.LocalTime]
    CellEncoder[java.time.ZonedDateTime]
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

    CellEncoder[java.net.URL].apply(new java.net.URL(
      "http://localhost:8080/path?a=b")) shouldBe "http://localhost:8080/path?a=b"
    CellEncoder[java.util.UUID].apply(UUID.fromString(
      "6f55090e-a807-49c2-a142-2a0db1f079df")) shouldBe "6f55090e-a807-49c2-a142-2a0db1f079df"
    CellEncoder[java.time.LocalTime]
      .apply(java.time.LocalTime.of(13, 4, 29)) shouldBe "13:04:29"
  }

}
