package fs2.data.csv

import cats.data.NonEmptyList

object Row {
  def apply(values: NonEmptyList[String]): Row = new Row(values, None)
  def unapply(arg: Row): Some[NonEmptyList[String]] = Some(arg.values)
}
