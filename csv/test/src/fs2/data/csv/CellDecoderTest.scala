package fs2.data.csv

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class CellDecoderTest extends AnyFlatSpec with Matchers with EitherValues {

  "CellDecoder" should "have implicit instances available for standard types" in {
    CellDecoder[String]
    CellDecoder[Array[Char]]
    CellDecoder[Boolean]
    CellDecoder[Unit]
    CellDecoder[Int]
    CellDecoder[BigDecimal]
    CellDecoder[FiniteDuration]
    CellDecoder[Duration]

    CellDecoder[java.net.URL]
    CellDecoder[java.util.UUID]
    CellDecoder[java.time.Instant]
    CellDecoder[java.time.LocalTime]
    CellDecoder[java.time.ZonedDateTime]

    CellDecoder[DecoderResult[Char]]
    CellDecoder[Either[String, Char]]
  }

  it should "decode standard types correctly" in {
    CellDecoder[Unit].apply("") shouldBe Right(())
    CellDecoder[Int].apply("78") shouldBe Right(78)
    CellDecoder[Boolean].apply("true") shouldBe Right(true)
    CellDecoder[Char].apply("C") shouldBe Right('C')
    CellDecoder[Double].apply("1.2") shouldBe Right(1.2)
    CellDecoder[BigDecimal].apply("1.2e456") shouldBe Right(BigDecimal("12e455"))
    CellDecoder[String].apply("foobar") shouldBe Right("foobar")
    CellDecoder[FiniteDuration].apply("2 seconds") shouldBe Right(2.seconds)

    CellDecoder[java.net.URL].apply("http://localhost:8080/path?a=b").isRight shouldBe true
    CellDecoder[java.util.UUID].apply("6f55090e-a807-49c2-a142-2a0db1f079df").map(_.toString) shouldBe Right("6f55090e-a807-49c2-a142-2a0db1f079df")
    CellDecoder[java.time.LocalTime].apply("13:04:29") shouldBe Right(java.time.LocalTime.of(13, 4, 29))
  }

  it should "handle container types properly" in {
    CellDecoder[DecoderResult[Char]].apply("G") shouldBe Right(Right('G'))
    CellDecoder[DecoderResult[Char]].apply("").map(_.isLeft) shouldBe Right(true)
    CellDecoder[Either[String, Char]].apply("F") shouldBe Right(Right('F'))
    CellDecoder[Either[String, Char]].apply("hello") shouldBe Right(Left("hello"))
  }

  it should "fail on invalid inputs" in {
    CellDecoder[Unit].apply("some random non empty string").isLeft shouldBe true
    CellDecoder[Int].apply("asdf").isLeft shouldBe true
    CellDecoder[Boolean].apply("maybe").isLeft shouldBe true
    CellDecoder[Char].apply("Chars").isLeft shouldBe true
    CellDecoder[Double].apply("-").isLeft shouldBe true
    CellDecoder[FiniteDuration].apply("2 meters").isLeft shouldBe true
  }

}
