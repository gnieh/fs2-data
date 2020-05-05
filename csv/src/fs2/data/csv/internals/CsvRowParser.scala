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
package csv
package internals

import cats.implicits._
import cats.data._

private[csv] object CsvRowParser {

  def pipe[F[_], Header](implicit F: RaiseThrowable[F],
                         Header: ParseableHeader[Header]): Pipe[F, NonEmptyList[String], CsvRow[Header]] =
    _.pull.uncons1.flatMap {
      case Some((firstRow, tail)) =>
        Header(firstRow) match {
          case Left(error) => Pull.raiseError[F](error)
          case Right(headers) if headers.length =!= firstRow.length =>
            val error = new HeaderError(
              s"Got ${headers.length} headers, but ${firstRow.length} columns. Both numbers must match!")
            Pull.raiseError[F](error)
          case Right(headers) => tail.map(CsvRow[Header](_, headers)).pull.echo
        }
      case None => Pull.done
    }.stream

}
