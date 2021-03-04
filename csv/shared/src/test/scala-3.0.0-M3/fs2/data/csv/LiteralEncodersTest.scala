package fs2.data.csv

import weaver._

object LiteralEncodersTest extends SimpleIOSuite {

  // The literal encoders should be present implicitly
  CellEncoder["foo"]
  CellEncoder[1]
  CellEncoder[true]
  CellEncoder['C']

  pureTest("The literal encoders should encode the precise same value") {
    expect(CellEncoder["foo"].apply("foo") == "foo") and
    expect(CellEncoder[1].apply(1) == "1") and
    expect(CellEncoder[true].apply(true) == "true") and
    expect(CellEncoder['C'].apply('C') == "C")
  }

}
