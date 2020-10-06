package fs2.data.csv

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues

class LiteralDecodersTest extends AnyFlatSpec with Matchers with EitherValues {

  "The literal decoders" should "be present implicitly" in {
    CellDecoder["foo"]
    CellDecoder[1]
    CellDecoder[true]
    CellDecoder['C']
  }

  it should "parse the precise same value" in {
    CellDecoder["foo"].apply("foo") shouldBe Right("foo")
    CellDecoder[1].apply("1") shouldBe Right(1)
    CellDecoder[true].apply("true") shouldBe Right(true)
    CellDecoder['C'].apply("C") shouldBe Right('C')
  }

  it should "fail for other values of the same primitive type" in {
    CellDecoder["foo"].apply("bar").isLeft shouldBe true
    CellDecoder[1].apply("2").isLeft shouldBe true
    CellDecoder[true].apply("false").isLeft shouldBe true
    CellDecoder['C'].apply("D").isLeft shouldBe true
  }

}
