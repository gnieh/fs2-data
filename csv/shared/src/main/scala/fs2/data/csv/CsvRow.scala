/*
 * Copyright 2022 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fs2.data.csv

import cats.data.NonEmptyList
import cats.syntax.all._

/** A CSV row with headers, that can be used to access the cell values.
  *
  * '''Note:''' the following invariant holds when using this class: `values` and `headers` have the same size.
  */
case class CsvRow[Header](override val values: NonEmptyList[String],
                          headers: NonEmptyList[Header],
                          override val line: Option[Long] = None)
    extends RowF[Some, Header](values, Some(headers), line)

object CsvRow {

  /** Constructs a [[CsvRow]] and checks that the size of values and headers match. */
  def apply[Header](values: NonEmptyList[String],
                    headers: NonEmptyList[Header],
                    line: Option[Long] = None): Either[CsvException, CsvRow[Header]] =
    if (values.length =!= headers.length)
      Left(
        new CsvException(
          s"Headers have size ${headers.length} but row has size ${values.length}. Both numbers must match!",
          line))
    else
      Right(new CsvRow(values, headers, line))

  def unsafe[Header](values: NonEmptyList[String], headers: NonEmptyList[Header]): CsvRow[Header] =
    apply(values, headers).fold(throw _, identity)

  def liftRow[Header](headers: NonEmptyList[Header])(row: Row): Either[CsvException, CsvRow[Header]] =
    apply(row.values, headers, row.line)

  def fromListHeaders[Header](l: List[(Header, String)]): Option[CsvRow[Header]] = {
    val (hs, vs) = l.unzip
    (NonEmptyList.fromList(vs), NonEmptyList.fromList(hs)).mapN((v, h) => new CsvRow(v, h))
  }

  def fromNelHeaders[Header](nel: NonEmptyList[(Header, String)]): CsvRow[Header] = {
    val (hs, vs) = nel.toList.unzip
    new CsvRow(NonEmptyList.fromListUnsafe(vs), NonEmptyList.fromListUnsafe(hs))
  }

  def unapply[Header](arg: CsvRow[Header]): Some[(NonEmptyList[String], NonEmptyList[Header])] = Some(
    (arg.values, arg.headers))
}
