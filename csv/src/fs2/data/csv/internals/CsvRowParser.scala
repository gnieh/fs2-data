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
import cats.data._

private[csv] object CsvRowParser {

  def pipe[F[_], Header](
      implicit F: ApplicativeError[F, Throwable],
      Header: ParseableHeader[Header]): Pipe[F, NonEmptyList[String], CsvRow[Header]] =
    _.mapAccumulate(Headers.Uninitialized[Header](): Headers[Header]) {
      case (Headers.Uninitialized(), fields) =>
        // headers have not been seen yet (first row)
        (Headers.Initialized(fields.map(Header.parse)), None)
      case (initialized @ Headers.Initialized(headers), fields) =>
        // otherwise, headers are already initialized properly, just pass them to the row
        (initialized, Some(new CsvRow[Header](fields, headers)))
    }.map(_._2).unNone

}
