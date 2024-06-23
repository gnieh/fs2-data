package fs2.data.csv.generic

import cats.data.NonEmptyList
import fs2.data.csv.CsvRow
import fs2.data.csv.generic.semiauto.deriveCsvRowDecoder
import weaver.SimpleIOSuite

object CsvRowDecoderDefaultsTest extends SimpleIOSuite {

  val csvRowDefaultI = CsvRow.unsafe(NonEmptyList.of("", "test", "42"), NonEmptyList.of("i", "s", "j"))
  val csvRowNoI =
    CsvRow.unsafe(NonEmptyList.of("test", "42"), NonEmptyList.of("s", "j"))
  val csvRowEmptyJ =
    CsvRow.unsafe(NonEmptyList.of("1", "test", ""), NonEmptyList.of("i", "s", "j"))

  case class Test(i: Int = 0, s: String, j: Option[Int])
  case class TestDefaults(i: Int = 7, j: String = "foo")

  val testDecoder = deriveCsvRowDecoder[Test]
  val testDefaultsDecoder = deriveCsvRowDecoder[TestDefaults]

  pureTest("case classes should be handled properly with default value and empty cell") {
    expect(testDecoder(csvRowDefaultI) == Right(Test(0, "test", Some(42))))
  }

  pureTest("case classes should be handled properly with default value and missing column") {
    expect(testDecoder(csvRowNoI) == Right(Test(0, "test", Some(42))))
  }

  pureTest("case classes should be handled properly with default string value and empty cell") {
    expect(testDefaultsDecoder(csvRowEmptyJ) == Right(TestDefaults(1, "foo")))
  }

}
