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

case class CsvRow[Header](values: NonEmptyList[String], header: Option[Header]) {

  private def byHeader[A](implicit ev: Header =:= NonEmptyList[A]): Option[Map[A, String]] =
    header.map(_.toList.zip(values.toList).toMap)

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

  /** Modifies the cell content at the given `header` using the function `f`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def modify[A](head: A)(f: String => String)(implicit ev: Header =:= NonEmptyList[A]): CsvRow[Header] =
    header match {
      case Some(header) => modify(header.toList.indexOf(head))(f)
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
      }, header)

  /** Returns the row with the cell at `header` modifed to `value`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def updated[A](head: Header, value: String)(implicit ev: Header =:= NonEmptyList[A]): CsvRow[Header] =
    header match {
      case Some(headers) => updated(headers.toList.indexOf(head), value)
      case None          => this
    }

  /** Returns the row without the cell at the given `idx`.
    * If the resulting row is empty, returns `None`.
    */
  def delete[A](idx: Int)(implicit ev: Header =:= NonEmptyList[A]): Option[CsvRow[NonEmptyList[A]]] =
    if (idx < 0 || idx >= values.size) {
      Some(this.asInstanceOf[CsvRow[NonEmptyList[A]]])
    } else {
      val (before, after) = values.toList.splitAt(idx)
      val newHeaders = header match {
        case None => Nil
        case _ =>
          val (h1, h2) = header.fold[List[A]](Nil)(_.toList).splitAt(idx)
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
  def delete[A](head: A)(implicit ev: Header =:= NonEmptyList[A]): Option[CsvRow[NonEmptyList[A]]] =
    header match {
      case Some(headers) => delete(headers.toList.indexOf(head))
      case None          => Some(this.asInstanceOf[CsvRow[NonEmptyList[A]]])
    }

  /** Returns the content of the cell at `headers` if it exists.
    * Returns `None` if `header` does not exist for the row.
    * An empty cell value results in `Some("")`.
    */
  def apply[A](head: A)(implicit ev: Header =:= NonEmptyList[A]): Option[String] =
    byHeader.flatMap(_.get(head))

  /** Returns a map representation of this row if headers are defined, otherwise
    * returns `None`.
    */
  def toMap[A](implicit ev: Header =:= NonEmptyList[A]): Option[Map[A, String]] =
    byHeader

}

object CsvRow {

  def fromList(l: List[String]): Option[CsvRow[Nothing]] =
    NonEmptyList.fromList(l).map(new CsvRow(_, None))

  def fromNel(nel: NonEmptyList[String]): CsvRow[Nothing] =
    CsvRow(nel, None)

  def fromListAndHeader[Header](l: List[String], header: Header): Option[CsvRow[Header]] =
    NonEmptyList.fromList(l).map(CsvRow(_, Some(header)))

  // this is almost only an alias for apply, here for consistency with fromListAndHeader
  def fromNelAndHeader[Header](nel: NonEmptyList[String], header: Header): CsvRow[Header] =
    CsvRow(nel, Some(header))

  def fromListHeaders[HeadElem](l: List[(HeadElem, String)]): Option[CsvRow[NonEmptyList[HeadElem]]] = {
    val (hs, vs) = l.unzip
    NonEmptyList.fromList(vs).map(CsvRow(_, NonEmptyList.fromList(hs)))
  }

  def fromNelHeaders[HeadElem](nel: NonEmptyList[(HeadElem, String)]): CsvRow[NonEmptyList[HeadElem]] = {
    val (hs, vs) = nel.toList.unzip
    CsvRow(NonEmptyList.fromListUnsafe(vs), NonEmptyList.fromList(hs))
  }

}
