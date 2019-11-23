package fs2.data.csv.generic

import scala.annotation.Annotation

/**
 * Mark an object of sealed trait to have a different representation in CSV than the name of the type.
 * @param name the name to expect in the CSV
 */
case class CsvName(name: String) extends Annotation
