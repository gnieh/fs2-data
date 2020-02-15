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

import cats._
import cats.implicits._
import cats.data._

import scala.language.higherKinds

private[csv] object CsvRowParser {

  def pipe[F[_], Header](withHeaders: Boolean)(
      implicit F: ApplicativeError[F, Throwable],
      Header: ParseableHeader[Header]): Pipe[F, NonEmptyList[String], CsvRow[Header]] =
    _.evalMapAccumulate[F, Headers[Header], Option[CsvRow[Header]]](Headers.Uninitialized[Header]()) {
      case (Headers.Uninitialized(), fields) if withHeaders =>
        // headers have not been seen yet (first row)
        F.fromEither(Header(fields)).map { headers =>
          Headers.Initialized(Some(headers)) ->  None
        }
      case (Headers.Uninitialized(), fields) =>
        // no header are to be parsed
        F.pure(Headers.Initialized[Header](None) -> Some(new CsvRow[Header](fields, None)))
      case (initialized @ Headers.Initialized(headers), fields) =>
        // otherwise, headers are already initialized properly, just pass them to the row
        F.pure(initialized -> Some(new CsvRow[Header](fields, headers)))
    }.map(_._2).unNone

}
