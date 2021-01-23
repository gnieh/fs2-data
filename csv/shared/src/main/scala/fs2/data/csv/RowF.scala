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

case class RowF[HC[a] <: HeaderContainer.Aux[a], Header](values: NonEmptyList[String], headers: HC[Header]) {

  /** Number of cells in the row. */
  def size: Int = values.size

  /** Returns the content of the cell at `header` if it exists.
    * Returns `None` if `header` isn't present.
    * An empty cell value results in `Some("")`.
    */
  def apply(header: Header): Option[String] =
    headers.findHeader(header).flatMap(values.get(_))

  /** Returns the decoded content of the cell at the given header.
    * Fails if the header doesn't exist or cannot be decoded
    * to the expected type.
    */
  def as[T](header: Header)(implicit decoder: CellDecoder[T]): DecoderResult[T] =
    headers.findHeader(header) match {
      case Some(v) => decoder(values.toList(v))
      case None    => Left(new DecoderError(s"unknown index $header"))
    }

  /** Modifies the cell content at the given `header` using the function `f`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def modify(header: Header)(f: String => String): RowF[HC, Header] =
    headers.findHeader(header).fold(this) { idx =>
      new RowF(values.zipWithIndex.map { case (cell, i) => if (i === idx) f(cell) else cell }, headers)
    }

  /** Returns the row with the cell at `header` set to `value`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be deleted. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def updated(header: Header, value: String): RowF[HC, Header] =
    modify(header)(_ => value)

  /** Returns the row without the cell at the given `idx`.
    * If the resulting row is empty, returns `None`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be deleted. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def delete(header: Header): Option[RowF[HC, Header]] =
    headers.findHeader(header).flatMap { index =>
      val (before, after) = values.toList.splitAt(index)
      (NonEmptyList.fromList(before ::: after.tail), headers.delete(header).asInstanceOf[Option[HC[Header]]])
        .mapN(new RowF[HC, Header](_, _))
    }

  /** Returns a map representation of this row if headers are defined, otherwise
    * returns `None`.
    */
  def toMap: Map[Header, String] =
    headers.toList.zip(values.toList).toMap
}
