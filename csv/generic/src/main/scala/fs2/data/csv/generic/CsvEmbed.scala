package fs2.data.csv.generic

import scala.annotation.Annotation

/** Mark a field of a case class to be embedded (= to be parsed from the same row, but inlined as value)
  */
case class CsvEmbed() extends Annotation
