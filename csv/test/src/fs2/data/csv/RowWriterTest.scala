package fs2.data.csv

import fs2.data.csv.internals.RowWriter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RowWriterTest extends AnyFlatSpec with Matchers {

  "RowWriter" should "escape according to the given escape mode" in {
    // separator
    RowWriter.encodeColumn(',', EscapeMode.Auto)(",") shouldBe "\",\""
    RowWriter.encodeColumn(',', EscapeMode.Always)(",") shouldBe "\",\""
    RowWriter.encodeColumn(',', EscapeMode.Never)(",") shouldBe ","

    // quotes
    RowWriter.encodeColumn(',', EscapeMode.Auto)("\"") shouldBe "\"\"\"\""
    RowWriter.encodeColumn(',', EscapeMode.Always)("\"") shouldBe "\"\"\"\""
    RowWriter.encodeColumn(',', EscapeMode.Never)("\"") shouldBe "\""

    // normal string
    RowWriter.encodeColumn(',', EscapeMode.Auto)("test") shouldBe "test"
    RowWriter.encodeColumn(',', EscapeMode.Always)("test") shouldBe "\"test\""
    RowWriter.encodeColumn(',', EscapeMode.Never)("test") shouldBe "test"
  }

}
