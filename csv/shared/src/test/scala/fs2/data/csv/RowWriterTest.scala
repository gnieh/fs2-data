package fs2.data.csv

import fs2.data.csv.internals.RowWriter

import weaver._

object RowWriterTest extends SimpleIOSuite {

  pureTest("RowWriter should escape according to the given escape mode") {
    // Needed for Scala 3.0.0-M3 :'(
    def test(mode: EscapeMode, in: String): String = RowWriter.encodeColumn(',', mode)(in)
    // separator
    expect(test(EscapeMode.Auto, ",") == "\",\"") and
      expect(test(EscapeMode.Always, ",") == "\",\"") and
      expect(test(EscapeMode.Never, ",") == ",") and
      // quotes
      expect(test(EscapeMode.Auto, "\"") == "\"\"\"\"") and
      expect(test(EscapeMode.Always, "\"") == "\"\"\"\"") and
      expect(test(EscapeMode.Never, "\"") == "\"") and
      // normal string
      expect(test(EscapeMode.Auto, "test") == "test") and
      expect(test(EscapeMode.Always, "test") == "\"test\"") and
      expect(test(EscapeMode.Never, "test") == "test")
  }

}
