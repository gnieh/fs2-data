package fs2.data.csv

import weaver._

object CellDecoderDerivationTest extends SimpleIOSuite {

  case class StringWrapper(value: String) derives CellDecoder
  case class TwoStringWrapper(value1: String, value2: String) derives CellDecoder

  enum Cases derives CellDecoder {
    case Foo
    case Bar
  }

  sealed trait Classic derives CellDecoder
  case object D extends Classic
  case object B extends Classic

//  given d: D.type = D
//  given b: B.type = B

  pureTest("derive for String wrappers") {
    expect(CellDecoder[StringWrapper].apply("foo") == Right(StringWrapper("foo")))
    //and expect(CellDecoder[TwoStringWrapper].apply("foo") == Right(TwoStringWrapper("foo", "bar")))
  }

  pureTest("derive for enums") {
    expect(CellDecoder[Cases].apply("Foo") == Right(Cases.Foo)) and
    expect(CellDecoder[Cases].apply("Bar") == Right(Cases.Bar)) and
    expect(CellDecoder[Classic].apply("D") == Right(D)) and
    expect(CellDecoder[Classic].apply("B") == Right(B))
  }
}
