/*
 * Copyright 2021 Lucas Satabin
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
import scala.annotation.nowarn

/** A CSV row with or without headers. The presence of headers is encoded via the first type param
  * which is a subtype of [[scala.Option]]. By preserving this information in types, it's possible to define
  * [[Row]] and [[CsvRow]] aliases as if they were plain case classes while keeping the code DRY.
  *
  * Operations on columns can always be performed using 0-based indices and additionally using a specified header value
  * if headers are present (and this fact statically known).
  *
  * '''Note:''' the following invariant holds when using this class: `values` and `headers` have the same size if headers are present.
  */
case class RowF[H[+a] <: Option[a], Header](values: NonEmptyList[String],
                                            headers: H[NonEmptyList[Header]],
                                            line: Option[Long] = None) {

  /** Number of cells in the row. */
  def size: Int = values.size

  /** Returns the content of the cell at `idx` if it exists.
    * Returns `None` if `idx` is out of row bounds.
    * An empty cell value results in `Some("")`.
    */
  def at(idx: Int): Option[String] =
    values.get(idx)

  /** Returns the decoded content of the cell at `idx`.
    * Fails if the index doesn't exist or cannot be decoded
    * to the expected type.
    */
  def asAt[T](idx: Int)(implicit decoder: CellDecoder[T]): DecoderResult[T] =
    values.get(idx) match {
      case Some(v) => decoder(v)
      case None    => Left(new DecoderError(s"unknown index $idx"))
    }

  /** Modifies the cell content at the given `idx` using the function `f`.
    */
  def modifyAt(idx: Int)(f: String => String): RowF[H, Header] =
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
    hasHeaders(modifyAt(headers.get.toList.indexOf(header))(f))

  /** Returns the row with the cell at `idx` modified to `value`. */
  def updatedAt(idx: Int, value: String): RowF[H, Header] =
    modifyAt(idx)(_ => value)

  /** Returns the row with the cell at `header` modified to `value`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def updated(header: Header, value: String)(implicit hasHeaders: HasHeaders[H, Header]): CsvRow[Header] =
    hasHeaders(updatedAt(headers.get.toList.indexOf(header), value))

  /** Returns the row with the cell at `header` modified to `value`.
    * If the header wasn't present in the row, it is added to the end of the fields.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def set(header: Header, value: String)(implicit hasHeaders: HasHeaders[H, Header]): CsvRow[Header] = {
    val idx = headers.get.toList.indexOf(header)
    if (idx < 0)
      hasHeaders(new RowF(values :+ value, headers.map(_ :+ header).asInstanceOf[H[NonEmptyList[Header]]]))
    else
      hasHeaders(updatedAt(idx, value))
  }

  /** Returns the row without the cell at the given `idx`.
    * If the resulting row is empty, returns `None`.
    */
  def deleteAt(idx: Int): Option[RowF[H, Header]] =
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
    deleteAt(headers.get.toList.indexOf(header)).map(hasHeaders)

  /** Returns the content of the cell at `header` if it exists.
    * Returns `None` if `header` does not exist for the row.
    * An empty cell value results in `Some("")`.
    */
  def apply(header: Header)(implicit hasHeaders: HasHeaders[H, Header]): Option[String] =
    byHeader.get(header): @nowarn("msg=HasHeaders")

  /** Returns the decoded content of the cell at `header`.
    * Fails if the field doesn't exist or cannot be decoded
    * to the expected type.
    */
  def as[T](header: Header)(implicit hasHeaders: HasHeaders[H, Header], decoder: CellDecoder[T]): DecoderResult[T] =
    (byHeader: @nowarn("msg=HasHeaders")).get(header) match {
      case Some(v) => decoder(v)
      case None    => Left(new DecoderError(s"unknown field $header"))
    }

  /** Returns a representation of this row as Map from headers to corresponding cell values.
    */
  def toMap(implicit hasHeaders: HasHeaders[H, Header]): Map[Header, String] =
    byHeader: @nowarn("msg=HasHeaders")

  /** Returns a representation of this row as NonEmptyMap from headers to corresponding cell values.
    */
  def toNonEmptyMap(implicit hasHeaders: HasHeaders[H, Header], order: Order[Header]): NonEmptyMap[Header, String] =
    headers.get.zip(values).toNem

  /** Drop all headers (if any).
    * @return a row without headers, but same values
    */
  def dropHeaders: Row = Row(values)

  // let's cache this to avoid recomputing it for every call to `as` or similar method
  // the `Option.get` call is safe since this field is only called in a context where a `HasHeaders`
  // instance is provided, meaning that the `Option` is `Some`
  // of course using a lazy val prevents us to make this constraint statistically checked, but
  // the gain is significant enough to allow for this local unsafety
  @deprecated("Have you checked that you have a `HasHeaders` instance in scope?")
  private lazy val byHeader: Map[Header, String] =
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
