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

import cats._
import cats.data._
import cats.implicits._

/** A CSV row with or without headers. The presence of headers is encoded via the first type param
  * which is a subtype of [[scala.Option]]. By preserving this information in types, it's possible to define
  * [[Row]] and [[CsvRow]] aliases as if they were plain case classes while keeping the code DRY.
  *
  * Operations on columns can always be performed using 0-based indices and additionally using a specified header value
  * if headers are present (and this fact statically known).
  *
  * '''Note:''' the following invariant holds when using this class: `values` and `headers` have the same size if headers are present.
  */
case class RowF[H[+a] <: Option[a], Header](values: NonEmptyList[String], headers: H[NonEmptyList[Header]]) {

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
  def modify(idx: Int)(f: String => String): RowF[H, Header] =
    if (idx < 0 || idx >= values.size)
      this
    else
      new RowF[H, Header](values.zipWithIndex.map { case (cell, i) => if (i === idx) f(cell) else cell }, headers)

  /** Modifies the cell content at the given `header` using the function `f`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def modify(header: Header)(f: String => String)(implicit hasHeaders: HasHeaders[H, Header]): CsvRow[Header] =
    modify(headers.get.toList.indexOf(header))(f)

  /** Returns the row with the cell at `idx` modified to `value`. */
  def updated(idx: Int, value: String): RowF[H, Header] =
    modify(idx)(_ => value)

  /** Returns the row with the cell at `header` modified to `value`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def updated(header: Header, value: String)(implicit hasHeaders: HasHeaders[H, Header]): CsvRow[Header] =
    updated(headers.get.toList.indexOf(header), value)

  /** Returns the row without the cell at the given `idx`.
    * If the resulting row is empty, returns `None`.
    */
  def delete(idx: Int): Option[RowF[H, Header]] =
    if (idx < 0 || idx >= values.size) {
      Some(this)
    } else {
      val (before, after) = values.toList.splitAt(idx)
      val nh = htraverse(headers) { headers =>
        val (h1, h2) = headers.toList.splitAt(idx)
        NonEmptyList.fromList(h1 ::: h2.tail)
      }
      (NonEmptyList.fromList(before ++ after.tail), nh).mapN(new RowF[H, Header](_, _))
    }

  /** Returns the row without the cell at the given `header`.
    * If the resulting row is empty, returns `None`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be deleted. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def delete(header: Header)(implicit hasHeaders: HasHeaders[H, Header]): Option[CsvRow[Header]] =
    delete(headers.get.toList.indexOf(header)).map(hasHeaders)

  /** Returns the content of the cell at `header` if it exists.
    * Returns `None` if `header` does not exist for the row.
    * An empty cell value results in `Some("")`.
    */
  def apply(header: Header)(implicit hasHeaders: HasHeaders[H, Header]): Option[String] =
    byHeader.get(header)

  /** Returns the decoded content of the cell at `header`.
    * Fails if the field doesn't exist or cannot be decoded
    * to the expected type.
    */
  def as[T](header: Header)(implicit hasHeaders: HasHeaders[H, Header], decoder: CellDecoder[T]): DecoderResult[T] =
    byHeader.get(header) match {
      case Some(v) => decoder(v)
      case None    => Left(new DecoderError(s"unknown field $header"))
    }

  /** Returns a map representation of this row if headers are defined, otherwise
    * returns `None`.
    */
  def toMap(implicit hasHeaders: HasHeaders[H, Header]): Map[Header, String] =
    byHeader

  /** Drop all headers (if any).
    * @return a row without headers, but same values
    */
  def dropHeaders: Row = Row(values)

  /** Concat this row with another. Header types must match, headers must be distinct.
    * @param other the row to append
    * @return a row combining both
    */
  def :::(other: RowF[H, Header]): RowF[H, Header] =
    new RowF[H, Header](
      values ::: other.values,
      (headers: Option[NonEmptyList[Header]], other.headers).mapN(_ ::: _).asInstanceOf[H[NonEmptyList[Header]]]
    )

  private def byHeader(implicit hasHeaders: HasHeaders[H, Header]): Map[Header, String] =
    headers.get.toList.zip(values.toList).toMap

  // Like Traverse[Option], but preserves the H type
  private def htraverse[G[_]: Applicative, A, B](h: H[A])(f: A => G[B]): G[H[B]] = h match {
    case Some(a) => f(a).map(Some(_)).asInstanceOf[G[H[B]]]
    case _       => Applicative[G].pure(None).asInstanceOf[G[H[B]]]
  }
}

object RowF {
  implicit object functor extends Functor[CsvRow[*]] {
    override def map[A, B](fa: CsvRow[A])(f: A => B): CsvRow[B] = fa.copy(headers = Some(fa.headers.get.map(f)))
  }
}
