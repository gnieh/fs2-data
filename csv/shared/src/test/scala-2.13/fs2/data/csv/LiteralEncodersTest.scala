package fs2.data.csv

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LiteralEncodersTest extends AnyFlatSpec with Matchers with EitherValues {

  "The literal encoders" should "be present implicitly" in {
    CellEncoder["foo"]
    CellEncoder[1]
    CellEncoder[true]
    CellEncoder['C']
  }

  it should "encode the precise same value" in {
    CellEncoder["foo"].apply("foo") shouldBe "foo"
    CellEncoder[1].apply(1) shouldBe "1"
    CellEncoder[true].apply(true) shouldBe "true"
    CellEncoder['C'].apply('C') shouldBe "C"
  }

}
