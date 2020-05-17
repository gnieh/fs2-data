/*
 * Copyright 2019 Lucas Satabin
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

import cats.data._
import cats.implicits._

class Row(val values: NonEmptyList[String]) {

  /** Number of cells in the row. */
  def size: Int = values.size

  /** Returns the content of the cell at `idx` if it exists.
    * Returns `None` if `idx` is out of row bounds.
    * An empty cell value results in `Some("")`.
    */
  def apply(idx: Int): Option[String] =
    values.get(idx)

  /** Returns the decoded content of the cell at `idx`.
    * Fails if the index doesn't exist or cannot be decoded
    * to the expected type.
    */
  def as[T](idx: Int)(implicit decoder: CellDecoder[T]): DecoderResult[T] =
    values.get(idx) match {
      case Some(v) => decoder(v)
      case None    => Left(new DecoderError(s"unknown index $idx"))
    }

  /** Modifies the cell content at the given `idx` using the function `f`.
    */
  def modify(idx: Int)(f: String => String): Row =
    if (idx < 0 || idx >= values.size)
      this
    else
      new Row(values.zipWithIndex.map {
        case (cell, i) =>
          if (i === idx)
            f(cell)
          else
            cell
      })

  /** Returns the row with the cell at `idx` set to `value`. */
  def updated(idx: Int, value: String): Row =
    modify(idx)(_ => value)

  /** Returns the row without the cell at the given `idx`.
    * If the resulting row is empty, returns `None`.
    */
  def delete(idx: Int): Option[Row] =
    if (idx < 0 || idx >= values.size) {
      Some(this)
    } else {
      val (before, after) = values.toList.splitAt(idx)
      NonEmptyList.fromList(before ++ after.tail).map(new Row(_))
    }
}

case class CsvRow[Header](override val values: NonEmptyList[String], headers: NonEmptyList[Header])
    extends Row(values) {

  private lazy val byHeader: Map[Header, String] =
    headers.toList.zip(values.toList).toMap

  /** Modifies the cell content at the given `idx` using the function `f`.
    */
  override def modify(idx: Int)(f: String => String): CsvRow[Header] =
    copy(values = super.modify(idx)(f).values)

  /** Modifies the cell content at the given `header` using the function `f`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def modify(header: Header)(f: String => String): CsvRow[Header] =
    modify(headers.toList.indexOf(header))(f)

  /** Returns the row with the cell at `idx` modified to `value`. */
  override def updated(idx: Int, value: String): CsvRow[Header] =
    modify(idx)(_ => value)

  /** Returns the row with the cell at `header` modified to `value`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def updated(header: Header, value: String): CsvRow[Header] =
    updated(headers.toList.indexOf(header), value)

  /** Returns the row without the cell at the given `idx`.
    * If the resulting row is empty, returns `None`.
    */
  override def delete(idx: Int): Option[CsvRow[Header]] =
    if (idx < 0 || idx >= values.size) {
      Some(this)
    } else {
      val (before, after) = values.toList.splitAt(idx)
      val (h1, h2) = headers.toList.splitAt(idx)
      (NonEmptyList.fromList(before ++ after.tail), NonEmptyList.fromList(h1 ++ h2.tail)).mapN(new CsvRow(_, _))
    }

  /** Returns the row without the cell at the given `header`.
    * If the resulting row is empty, returns `None`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be deleted. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def delete(header: Header): Option[CsvRow[Header]] =
    delete(headers.toList.indexOf(header))

  /** Returns the content of the cell at `header` if it exists.
    * Returns `None` if `header` does not exist for the row.
    * An empty cell value results in `Some("")`.
    */
  def apply(header: Header): Option[String] =
    byHeader.get(header)

  /** Returns the decoded content of the cell at `header`.
    * Fails if the field doesn't exist or cannot be decoded
    * to the expected type.
    */
  def as[T](header: Header)(implicit decoder: CellDecoder[T]): DecoderResult[T] =
    byHeader.get(header) match {
      case Some(v) => decoder(v)
      case None    => Left(new DecoderError(s"unknown field $header"))
    }

  /** Returns a map representation of this row if headers are defined, otherwise
    * returns `None`.
    */
  def toMap: Map[Header, String] =
    byHeader

}

object CsvRow {

  def fromListHeaders[Header](l: List[(Header, String)]): Option[CsvRow[Header]] = {
    val (hs, vs) = l.unzip
    (NonEmptyList.fromList(vs), NonEmptyList.fromList(hs)).mapN(new CsvRow(_, _))
  }

  def fromNelHeaders[Header](nel: NonEmptyList[(Header, String)]): CsvRow[Header] = {
    val (hs, vs) = nel.toList.unzip
    new CsvRow(NonEmptyList.fromListUnsafe(vs), NonEmptyList.fromListUnsafe(hs))
  }

}
