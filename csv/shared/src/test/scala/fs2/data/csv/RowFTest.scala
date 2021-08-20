package fs2.data.csv

import weaver._
import cats.data.NonEmptyList

object RowFTest extends SimpleIOSuite {

  pureTest("RowF.set should change the value at existing header") {
    val row = CsvRow.unsafe(NonEmptyList.of("1", "2", "3"), NonEmptyList.of("a", "b", "c"))
    expect.eql(NonEmptyList.of("1", "4", "3"), row.set("b", "4").values)
  }

  pureTest("RowF.set should add the value at end of row in case of missing header") {
    val row = CsvRow.unsafe(NonEmptyList.of("1", "2", "3"), NonEmptyList.of("a", "b", "c"))
    val extended = row.set("d", "4")
    expect.eql(NonEmptyList.of("1", "2", "3", "4"), extended.values) and
      expect.eql(NonEmptyList.of("a", "b", "c", "d"), extended.headers.get)
  }

}
