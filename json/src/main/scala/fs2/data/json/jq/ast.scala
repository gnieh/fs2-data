/*
 * Copyright 2023 Lucas Satabin
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

import cats.data.NonEmptyChain
import cats.kernel.Monoid

sealed trait Jq
sealed trait Filter extends Jq {
  def ~(that: Filter): Filter =
    (this, that) match {
      case (Jq.Identity, _)                      => that
      case (_, Jq.Identity)                      => this
      case (Jq.Sequence(s1), Jq.Sequence(s2))    => Jq.Sequence(s1 ++ s2)
      case (Jq.Sequence(s1), that: SimpleFilter) => Jq.Sequence(s1 :+ that)
      case (f: SimpleFilter, Jq.Sequence(s2))    => Jq.Sequence(f +: s2)
      case (f1: SimpleFilter, f2: SimpleFilter)  => Jq.Sequence(NonEmptyChain(f1, f2))
    }

}

object Filter {
  implicit object monoid extends Monoid[Filter] {
    override def combine(x: Filter, y: Filter): Filter = x ~ y
    override def empty: Filter = Jq.Identity
  }
}
sealed trait SimpleFilter extends Filter

sealed trait Constructor extends Jq
object Jq {
  // filters
  case object Root extends SimpleFilter
  case object Identity extends SimpleFilter
  final case class Field(name: String) extends SimpleFilter
  final case class Index(idx: Int) extends SimpleFilter
  final case class Slice(start: Int, end: Option[Int]) extends SimpleFilter
  final case object RecursiveDescent extends SimpleFilter
  final case class Sequence(jqs: NonEmptyChain[SimpleFilter]) extends Filter

  final case class Iterator(filter: Filter, inner: Jq) extends Jq

  // constructors
  final case class Arr(prefix: Filter, values: List[Jq]) extends Constructor
  final case class Obj(prefix: Filter, fields: List[(String, Jq)]) extends Constructor
  final case class Num(n: String) extends Constructor
  final case class Str(s: String) extends Constructor
  final case class Bool(b: Boolean) extends Constructor
  case object Null extends Constructor

}
