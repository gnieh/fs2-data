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

case class CsvRow[Header] private[csv] (values: NonEmptyList[String], header: Option[Header]) {

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
      }, header)

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
      }, header)

}

object CsvRow {

  def fromList(l: List[String]): Option[CsvRow[Nothing]] =
    NonEmptyList.fromList(l).map(new CsvRow(_, None))

  def fromNel(nel: NonEmptyList[String]): CsvRow[Nothing] =
    CsvRow(nel, None)

  def fromListAndHeader[Header](l: List[String], header: Header): Option[CsvRow[Header]] =
    NonEmptyList.fromList(l).map(CsvRow(_, Some(header)))

  def fromNelAndHeader[Header](nel: NonEmptyList[String], header: Header): CsvRow[Header] =
    CsvRow(nel, Some(header))

  def fromListHeaders[HeadElem](l: List[(HeadElem, String)]): Option[CsvNelRow[HeadElem]] = {
    val (hs, vs) = l.unzip
    NonEmptyList.fromList(vs).map(CsvRow(_, NonEmptyList.fromList(hs)))
  }

  def fromNelHeaders[HeadElem](nel: NonEmptyList[(HeadElem, String)]): CsvNelRow[HeadElem] = {
    val (hs, vs) = nel.toList.unzip
    CsvRow(NonEmptyList.fromListUnsafe(vs), NonEmptyList.fromList(hs))
  }

  implicit class CsvRowNelOps[HeadElem](row: CsvNelRow[HeadElem]) {
    private def byHeader: Option[Map[HeadElem, String]] =
      row.header.map(_.toList.zip(row.values.toList).toMap)

    /** Modifies the cell content at the given `header` using the function `f`.
     *
     * **Note:** Only the first occurrence of the values with the given header
     * will be modified. It shouldn't be a problem in the general case as headers
     * should not be duplicated.
     */
    def modify(head: HeadElem)(f: String => String): CsvNelRow[HeadElem] =
      row.header match {
        case Some(header) => row.modify(header.toList.indexOf(head))(f)
        case None         => row
      }

    /** Returns the row with the cell at `header` modified to `value`.
     *
     * **Note:** Only the first occurrence of the values with the given header
     * will be modified. It shouldn't be a problem in the general case as headers
     * should not be duplicated.
     */
    def updated(head: HeadElem, value: String): CsvNelRow[HeadElem] =
      row.header match {
        case Some(headers) => row.updated(headers.toList.indexOf(head), value)
        case None          => row
      }

    /** Returns the row without the cell at the given `idx`.
     * If the resulting row is empty, returns `None`.
     */
    def delete(idx: Int): Option[CsvNelRow[HeadElem]] =
      if (idx < 0 || idx >= row.values.size) {
        Some(row)
      } else {
        val (before, after) = row.values.toList.splitAt(idx)
        val newHeaders = row.header match {
          case None => Nil
          case _ =>
            val (h1, h2) = row.header.fold[List[HeadElem]](Nil)(_.toList).splitAt(idx)
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
    def delete(head: HeadElem): Option[CsvNelRow[HeadElem]] =
      row.header match {
        case Some(headers) => delete(headers.toList.indexOf(head))
        case None          => Some(this.asInstanceOf[CsvNelRow[HeadElem]])
      }

    /** Returns the content of the cell at `headers` if it exists.
     * Returns `None` if `header` does not exist for the row.
     * An empty cell value results in `Some("")`.
     */
    def apply(head: HeadElem): Option[String] =
      byHeader.flatMap(_.get(head))

    /** Returns a map representation of this row if headers are defined, otherwise
     * returns `None`.
     */
    def toMap: Option[Map[HeadElem, String]] =
      byHeader
  }

}
