package fs2.data.csv

sealed trait HasHeaders[H[a] <: Option[a], Header] extends (RowF[H, Header] => CsvRow[Header])

object HasHeaders {
  implicit def hasHeaders[Header]: HasHeaders[Some, Header] = new HasHeaders[Some, Header] {
    override def apply(value: RowF[Some, Header]): CsvRow[Header] = value
  }
}
