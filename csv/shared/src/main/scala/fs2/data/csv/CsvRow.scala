package fs2.data.csv

import cats.data.NonEmptyList
import cats.implicits._

object CsvRow {

  /** Constructs a [[CsvRow]] and checks that the size of values and headers match. */
  def apply[Header](values: NonEmptyList[String], headers: NonEmptyList[Header]): Either[CsvException, CsvRow[Header]] =
    if (values.length =!= headers.length)
      Left(
        new CsvException(
          s"Headers have size ${headers.length} but row has size ${values.length}. Both numbers must match!"))
    else
      Right(new CsvRow(values, Some(headers)))

  def unsafe[Header](values: NonEmptyList[String], headers: NonEmptyList[Header]): CsvRow[Header] =
    apply(values, headers).fold(throw _, identity)

  def liftRow[Header](headers: NonEmptyList[Header])(row: Row): Either[CsvException, CsvRow[Header]] =
    apply(row.values, headers)

  def fromListHeaders[Header](l: List[(Header, String)]): Option[CsvRow[Header]] = {
    val (hs, vs) = l.unzip
    (NonEmptyList.fromList(vs), NonEmptyList.fromList(hs)).mapN((v, h) => new CsvRow(v, Some(h)))
  }

  def fromNelHeaders[Header](nel: NonEmptyList[(Header, String)]): CsvRow[Header] = {
    val (hs, vs) = nel.toList.unzip
    new CsvRow(NonEmptyList.fromListUnsafe(vs), Some(NonEmptyList.fromListUnsafe(hs)))
  }

  def unapply[Header](arg: CsvRow[Header]): Some[(NonEmptyList[String], NonEmptyList[Header])] = Some(
    (arg.values, arg.headers.get))
}
