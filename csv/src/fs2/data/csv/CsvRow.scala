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

class CsvRow[Header](val values: NonEmptyList[String], val headers: Option[NonEmptyList[Header]]) {

  private lazy val byHeader: Option[Map[Header, String]] =
    headers.map(_.toList.zip(values.toList).toMap)

  /** Number of cells in the row. */
  def size: Int = values.size

  /** Returns the content of the cell at `idx` if it exists.
    * Returns `None` if `idx` is out of row bounds.
    * An empty cell value results in `Some("")`.
    */
  def apply(idx: Int): Option[String] =
    values.get(idx)

  /** Modifies the cell content at the given `idx` using the function `f`.
    */
  def modify(idx: Int)(f: String => String): CsvRow[Header] =
    if (idx < 0 || idx >= values.size)
      this
    else
      new CsvRow(values.zipWithIndex.map {
        case (cell, i) =>
          if (i === idx)
            f(cell)
          else
            cell
      }, headers)

  /** Modifies the cell content at the given `header` using the function `f`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def modify(header: Header)(f: String => String): CsvRow[Header] =
    headers match {
      case Some(headers) => modify(headers.toList.indexOf(header))(f)
      case None          => this
    }

  /** Returns the row with the cell at `idx` modifed to `value`. */
  def updated(idx: Int, value: String): CsvRow[Header] =
    if (idx < 0 || idx >= values.size)
      this
    else
      new CsvRow(values.zipWithIndex.map {
        case (cell, i) =>
          if (i === idx)
            value
          else
            cell
      }, headers)

  /** Returns the row with the cell at `header` modifed to `value`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def updated(header: Header, value: String): CsvRow[Header] =
    headers match {
      case Some(headers) => updated(headers.toList.indexOf(header), value)
      case None          => this
    }

  /** Returns the row without the cell at the given `idx`.
    * If the resulting row is empty, returns `None`.
    */
  def delete(idx: Int): Option[CsvRow[Header]] =
    if (idx < 0 || idx >= values.size) {
      Some(this)
    } else {
      val (before, after) = values.toList.splitAt(idx)
      val newHeaders = headers match {
        case None => Nil
        case _ =>
          val (h1, h2) = headers.fold[List[Header]](Nil)(_.toList).splitAt(idx)
          h1 ++ h2.tail
      }
      NonEmptyList.fromList(before ++ after.tail).map(new CsvRow(_, NonEmptyList.fromList(newHeaders)))
    }

  /** Returns the row without the cell at the given `header`.
    * If the resulting row is empty, returns `None`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be deleted. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def delete(header: Header): Option[CsvRow[Header]] =
    headers match {
      case Some(headers) => delete(headers.toList.indexOf(header))
      case None          => Some(this)
    }

  /** Returns the content of the cell at `headers` if it exists.
    * Returns `None` if `header` does not exist for the row.
    * An empty cell value results in `Some("")`.
    */
  def apply(header: Header): Option[String] =
    byHeader.flatMap(_.get(header))

  /** Returns a map representation of this row if headers are defined, otherwise
    * returns `None`.
    */
  def toMap: Option[Map[Header, String]] =
    byHeader

}

object CsvRow {

  def fromList(l: List[String]): Option[CsvRow[Nothing]] =
    NonEmptyList.fromList(l).map(new CsvRow(_, None))

  def fromNel(nel: NonEmptyList[String]): CsvRow[Nothing] =
    new CsvRow(nel, None)

  def fromListHeaders[Header](l: List[(Header, String)]): Option[CsvRow[Header]] = {
    val (hs, vs) = l.unzip
    NonEmptyList.fromList(vs).map(new CsvRow(_, NonEmptyList.fromList(hs)))
  }

  def fromNelHeaders[Header](nel: NonEmptyList[(Header, String)]): CsvRow[Header] = {
    val (hs, vs) = nel.toList.unzip
    new CsvRow(NonEmptyList.fromListUnsafe(vs), NonEmptyList.fromList(hs))
  }

}
