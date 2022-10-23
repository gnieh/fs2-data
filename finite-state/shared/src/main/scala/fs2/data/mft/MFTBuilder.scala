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

package fs2.data.mft

import scala.collection.mutable.ListBuffer

class MFTBuilder[Guard, InTag, OutTag] private[mft] {
  self =>

  private[mft] var initial = 0

  private[mft] val states = new ListBuffer[StateBuilder]

  final class StateBuilder private[mft] (val q: Int, val nargs: Int) {
    val rules = new ListBuffer[(EventSelector[Guard, InTag], Rhs[OutTag])]
    def apply(pat: PatternBuilder): RuleBuilder =
      new RuleBuilder(this, pat)

    def apply(f: Forest, args: Rhs[OutTag]*): Rhs[OutTag] =
      Rhs.Call(q, f, args.toList)
  }

  sealed trait PatternBuilder
  sealed trait Guardable extends PatternBuilder {
    def when(guard: Guard): PatternBuilder
  }
  private[mft] object PatternBuilder {
    case class Any(guard: Option[Guard]) extends Guardable {
      override def when(guard: Guard): PatternBuilder = Any(Some(guard))
    }
    case class Node(in: InTag, guard: Option[Guard]) extends Guardable {
      override def when(guard: Guard): PatternBuilder = Node(in, Some(guard))
    }
    case class AnyNode(guard: Option[Guard]) extends Guardable {
      override def when(guard: Guard): PatternBuilder = AnyNode(Some(guard))
    }
    case class Leaf(in: InTag, guard: Option[Guard]) extends Guardable {
      override def when(guard: Guard): PatternBuilder = Leaf(in, Some(guard))
    }
    case class AnyLeaf(guard: Option[Guard]) extends Guardable {
      override def when(guard: Guard): PatternBuilder = AnyLeaf(Some(guard))
    }
    case object Epsilon extends PatternBuilder
  }

  class RuleBuilder private[mft] (q: StateBuilder, pat: PatternBuilder) {

    def ->(rhs: Rhs[OutTag]): Unit = {
      pat match {
        case PatternBuilder.Node(in, g) => q.rules += (EventSelector.Node(in, g) -> rhs)
        case PatternBuilder.AnyNode(g)  => q.rules += (EventSelector.AnyNode(g) -> rhs)
        case PatternBuilder.Leaf(in, g) => q.rules += (EventSelector.Leaf(in, g) -> rhs)
        case PatternBuilder.AnyLeaf(g)  => q.rules += (EventSelector.AnyLeaf(g) -> rhs)
        case PatternBuilder.Epsilon     => q.rules += (EventSelector.Epsilon() -> rhs)
        case PatternBuilder.Any(g) =>
          q.rules += (EventSelector.AnyNode(g) -> rhs) += (EventSelector.AnyLeaf(g) -> rhs) += (EventSelector
            .Epsilon() -> rhs)
      }
      () // to silence discard warnings
    }

  }

  def build: MFT[Guard, InTag, OutTag] =
    new MFT(initial, states.map { st => st.q -> Rules(List.range(0, st.nargs), st.rules.result()) }.toMap)

}
