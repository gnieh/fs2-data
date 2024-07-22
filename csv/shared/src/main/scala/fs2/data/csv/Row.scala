/*
 * Copyright 2024 fs2-data Project
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

import cats.syntax.all._

/** A CSV row
  * 
  * Operations on columns can be performed using 0-based indices
  */
case class Row(values: List[String],
                                            line: Option[Long] = None) {

  /** Number of cells in the row. */
  def size: Int = values.size

  /**
    * Set the line number for this row.
    */
  def withLine(line: Option[Long]): Row =
    copy(line = line)

  /** Returns the content of the cell at `idx` if it exists.
    * Returns `None` if `idx` is out of row bounds.
    * An empty cell value results in `Some("")`.
    */
  def at(idx: Int): Option[String] =
    values.get(idx.toLong)

  /** Returns the decoded content of the cell at `idx`.
    * Fails if the index doesn't exist or cannot be decoded
    * to the expected type.
    */
  def asAt[T](idx: Int)(implicit decoder: CellDecoder[T]): DecoderResult[T] =
    values.get(idx.toLong) match {
      case Some(v) => decoder(v)
      case None    => Left(new DecoderError(s"unknown index $idx"))
    }

  /** Returns the decoded content of the cell at `idx` wrapped in Some if the cell is non-empty, None otherwise.
    * Fails if the index doesn't exist or cannot be decoded
    * to the expected type.
    */
  @deprecated(message =
                "Use `RowF.asOptionalAt` instead, as it gives more flexibility and has the same default behavior.",
              since = "fs2-data 1.7.0")
  def asNonEmptyAt[T](idx: Int)(implicit decoder: CellDecoder[T]): DecoderResult[Option[T]] =
    asOptionalAt(idx)

  /** Returns the decoded content of the cell at `idx` wrapped in Some if the cell is non-empty, `None` otherwise.
    * The meaning of _empty_ can be tuned by setting providing a custom `isEmpty` predicate (by default, matches the empty string).
    * In case the index does not exist, the `missing` parameter defines the behavior (by default, it faile)
    * Fails if the index cannot be decoded to the expected type.
    */
  def asOptionalAt[T](
      idx: Int,
      missing: Int => DecoderResult[Option[T]] = (idx: Int) => Left(new DecoderError(s"unknown index $idx")),
      isEmpty: String => Boolean = _.isEmpty)(implicit decoder: CellDecoder[T]): DecoderResult[Option[T]] =
    values.get(idx.toLong) match {
      case Some(v) if isEmpty(v) => Right(None)
      case Some(v)               => decoder.apply(v).map(Some(_))
      case None                  => missing(idx)
    }

  /** Modifies the cell content at the given `idx` using the function `f`.
    */
  def modifyAt(idx: Int)(f: String => String): Row =
    if (idx < 0 || idx >= values.size)
      this
    else
      new Row(values.zipWithIndex.map { case (cell, i) => if (i === idx) f(cell) else cell })

  /** Returns the row with the cell at `idx` modified to `value`. */
  def updatedAt(idx: Int, value: String): Row =
    modifyAt(idx)(_ => value)

  /** Returns the row without the cell at the given `idx`.
    * If the resulting row is empty, returns `None`.
    */
  def deleteAt(idx: Int): Row =
    if (idx < 0 || idx >= values.size) {
      this
    } else {
      val (before, after) = values.toList.splitAt(idx)
      this.copy(values = before ++ after.tail)
    }

}

object Row {
  def unapply(row: Row): Option[List[String]] =
    Some(row.values)
}
