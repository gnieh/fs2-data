package fs2.data.csv.generic

import scala.annotation.Annotation

/**
  * Mark a field of a case class to have a different name in the CSV when deriving a codec.
  * @param name the name to expect in the CSV
  */
case class CsvName(name: String) extends Annotation
