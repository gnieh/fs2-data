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
package fs2
package data

import csv.internals._

import cats._
import cats.data._

import scala.language.higherKinds

package object csv {

  /** Transforms a stream of characters into a stream of CSV rows.
    */
  def rows[F[_]](separator: Char = ',')(
      implicit F: ApplicativeError[F, Throwable]): Pipe[F, Char, NonEmptyList[String]] =
    RowParser.pipe[F](separator)

  /** Transforms a stream of raw CSV rows into parsed CSV rows with headers. */
  def headers[F[_], Header](implicit F: ApplicativeError[F, Throwable],
                            Header: ParseableHeader[Header]): Pipe[F, NonEmptyList[String], CsvRow[Header]] =
    CsvRowParser.pipe[F, Header](true)

  /** Transforms a stream of raw CSV rows into parsed CSV rows with headers. */
  def noHeaders[F[_]](implicit F: ApplicativeError[F, Throwable]): Pipe[F, NonEmptyList[String], CsvRow[Nothing]] =
    CsvRowParser.pipe[F, Nothing](false)

}
