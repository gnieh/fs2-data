package fs2.data.csv

import fs2.data.csv.internals.RowWriter

import weaver._

object RowWriterTest extends SimpleIOSuite {

  pureTest("RowWriter should escape according to the given escape mode") {
    // separator
    expect.eql(RowWriter.encodeColumn(',', EscapeMode.Auto)(","), "\",\"") and
      expect.eql(RowWriter.encodeColumn(',', EscapeMode.Always)(","), "\",\"") and
      expect.eql(RowWriter.encodeColumn(',', EscapeMode.Never)(","), ",") and
      // quotes
      expect.eql(RowWriter.encodeColumn(',', EscapeMode.Auto)("\""), "\"\"\"\"") and
      expect.eql(RowWriter.encodeColumn(',', EscapeMode.Always)("\""), "\"\"\"\"") and
      expect.eql(RowWriter.encodeColumn(',', EscapeMode.Never)("\""), "\"") and
      // normal string
      expect.eql(RowWriter.encodeColumn(',', EscapeMode.Auto)("test"), "test") and
      expect.eql(RowWriter.encodeColumn(',', EscapeMode.Always)("test"), "\"test\"") and
      expect.eql(RowWriter.encodeColumn(',', EscapeMode.Never)("test"), "test")
  }

}
