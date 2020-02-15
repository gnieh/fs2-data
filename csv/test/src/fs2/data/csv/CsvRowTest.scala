package fs2.data.csv

import cats.data.NonEmptyList
import org.scalatest.{FlatSpec, Matchers}

class CsvRowTest extends FlatSpec with Matchers {

  "CsvRow" should "allow simple operations for any Header type" in {
    // Unit is a good placeholder for some opaque user type
    val testRow = CsvRow.fromNelAndHeader(NonEmptyList.of("1", "3", "5"), ())
    testRow.size should be(3)
    testRow(2) should contain ("5")
    val modified = testRow.modify(2)(_ * 2)
    modified(2) should contain ("55")
    val updated = testRow.updated(1, "9")
    updated(1) should contain("9")
  }

  "CsvRow" should "allow advanced operations for Nel-based Header types (CsvNelRow)" in {
    val in = NonEmptyList.of("A" -> "1", "B" -> "3", "C" -> "5")
    val testRow = CsvRow.fromNelHeaders(in)
    testRow.size should be(3)
    testRow("C") should contain ("5")
    val modified = testRow.modify("C")(_ * 2)
    modified("C") should contain ("55")
    val updated = testRow.updated("A", "9")
    updated(0) should contain("9")
    testRow.toMap should contain(in.toList.toMap)
  }

}
