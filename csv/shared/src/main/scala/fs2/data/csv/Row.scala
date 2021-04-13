package fs2.data.csv

import cats.data.NonEmptyList

object Row {
  def apply(values: NonEmptyList[String], line: Option[Long] = None): Row = new Row(values, None, line)
  def unapply(arg: Row): Some[NonEmptyList[String]] = Some(arg.values)
}
