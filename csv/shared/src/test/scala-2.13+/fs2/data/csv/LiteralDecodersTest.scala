package fs2.data.csv

import weaver._

object LiteralDecodersTest extends SimpleIOSuite {

  // The literal decoders should be present implicitly
  CellDecoder["foo"]
  CellDecoder[1]
  CellDecoder[true]
  CellDecoder['C']

  pureTest("The literal decoders should parse the precise same value") {
    expect(CellDecoder["foo"].apply("foo") == Right("foo")) and
      expect(CellDecoder[1].apply("1") == Right(1)) and
      expect(CellDecoder[true].apply("true") == Right(true)) and
      expect(CellDecoder['C'].apply("C") == Right('C'))
  }

  pureTest("The literal decoders should fail for other values of the same primitive type") {
    expect(CellDecoder["foo"].apply("bar").isLeft == true) and
      expect(CellDecoder[1].apply("2").isLeft == true) and
      expect(CellDecoder[true].apply("false").isLeft == true) and
      expect(CellDecoder['C'].apply("D").isLeft == true)
  }

}
