/*
 * Copyright 2022 Lucas Satabin
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

package fs2.data.json.jq

import cats.data.NonEmptyList

sealed trait Jq
sealed trait Filter extends Jq
sealed trait Constructor extends Jq
object Jq {
  // filters
  final case object Identity extends Filter
  final case class Field(name: String) extends Filter
  final case class Index(idx: Int) extends Filter
  final case class Slice(start: Int, end: Option[Int]) extends Filter
  final case object RecursiveDescent extends Filter
  final case class Sequence(jqs: NonEmptyList[Filter]) extends Filter

  final case class Iterator(filter: Filter, inner: Jq) extends Jq

  // constructors
  final case class Arr(values: List[Jq]) extends Constructor
  final case class Obj(fields: List[(String, Jq)]) extends Constructor
  final case class Num(n: BigDecimal) extends Constructor
  final case class Str(s: String) extends Constructor
  final case class Bool(b: Boolean) extends Constructor
  final case object Null extends Constructor

}
