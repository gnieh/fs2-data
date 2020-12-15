package fs2.data.csv

import fs2.data.csv.internals.RowWriter

import weaver._

object RowWriterTest extends SimpleIOSuite {

  pureTest("RowWriter should escape according to the given escape mode") {
    // separator
    expect(RowWriter.encodeColumn(',', EscapeMode.Auto)(",") == "\",\"") and
      expect(RowWriter.encodeColumn(',', EscapeMode.Always)(",") == "\",\"") and
      expect(RowWriter.encodeColumn(',', EscapeMode.Never)(",") == ",") and
      // quotes
      expect(RowWriter.encodeColumn(',', EscapeMode.Auto)("\"") == "\"\"\"\"") and
      expect(RowWriter.encodeColumn(',', EscapeMode.Always)("\"") == "\"\"\"\"") and
      expect(RowWriter.encodeColumn(',', EscapeMode.Never)("\"") == "\"") and
      // normal string
      expect(RowWriter.encodeColumn(',', EscapeMode.Auto)("test") == "test") and
      expect(RowWriter.encodeColumn(',', EscapeMode.Always)("test") == "\"test\"") and
      expect(RowWriter.encodeColumn(',', EscapeMode.Never)("test") == "test")
  }

}
