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

class CsvRow[Header](val values: NonEmptyList[String], val headers: Option[NonEmptyList[Header]]) {

  private lazy val byHeader: Option[Map[Header, String]] =
    headers.map(_.toList.zip(values.toList).toMap)

  def apply(idx: Int): Option[String] =
    values.get(idx)

  def apply(header: Header): Option[String] =
    byHeader.flatMap(_.get(header))

  def toMap: Option[Map[Header, String]] =
    byHeader

}
