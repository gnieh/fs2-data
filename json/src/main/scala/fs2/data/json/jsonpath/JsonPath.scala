/*
 * Copyright 2019-2022 Lucas Satabin
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

package fs2.data.json.jsonpath

import cats.data.NonEmptyList

case class JsonPath(locations: NonEmptyList[Location])
sealed trait Location
object Location {
  case class Child(child: Property) extends Location
  case class Descendent(child: Property) extends Location
  case class Pred(predicate: Predicate) extends Location
}

sealed trait Property
object Property {
  case class Name(n: String) extends Property
  case object Wildcard extends Property
}

sealed trait Predicate
object Predicate {
  case class Index(idx: Int) extends Predicate
  case class Range(lower: Int, upper: Option[Int]) extends Predicate
  case object Wildcard extends Predicate
}
