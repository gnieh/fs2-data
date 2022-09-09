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

package fs2.data.mft

import scala.collection.mutable.ListBuffer

class MFTBuilder[InTag, OutTag] private[mft] {
  self =>

  private[mft] var initial = 0

  private[mft] val states = new ListBuffer[StateBuilder]

  final class StateBuilder private[mft] (val q: Int, val nargs: Int) {
    val rules = new ListBuffer[(EventSelector[InTag], Rhs[OutTag])]
    def apply(pat: PatternBuilder): RuleBuilder =
      new RuleBuilder(this, pat)

    def apply(f: Forest, args: Rhs[OutTag]*): Rhs[OutTag] =
      Rhs.Call(q, f, args.toList)
  }

  sealed trait PatternBuilder
  private[mft] object PatternBuilder {
    case object Any extends PatternBuilder
    case class Node(in: InTag) extends PatternBuilder
    case object AnyNode extends PatternBuilder
    case class Leaf(in: InTag) extends PatternBuilder
    case object AnyLeaf extends PatternBuilder
    case object Epsilon extends PatternBuilder
  }

  class RuleBuilder private[mft] (q: StateBuilder, pat: PatternBuilder) {

    def ->(rhs: Rhs[OutTag]): Unit =
      pat match {
        case PatternBuilder.Node(in) => q.rules += (EventSelector.Node(in) -> rhs)
        case PatternBuilder.AnyNode  => q.rules += (EventSelector.AnyNode -> rhs)
        case PatternBuilder.Leaf(in) => q.rules += (EventSelector.Leaf(in) -> rhs)
        case PatternBuilder.AnyLeaf  => q.rules += (EventSelector.AnyLeaf -> rhs)
        case PatternBuilder.Epsilon  => q.rules += (EventSelector.Epsilon -> rhs)
        case PatternBuilder.Any =>
          q.rules += (EventSelector.AnyNode -> rhs) += (EventSelector.AnyLeaf -> rhs) += (EventSelector.Epsilon -> rhs)
      }

  }

  def build: MFT[InTag, OutTag] =
    new MFT(initial, states.map { st => st.q -> Rules(List.range(0, st.nargs), st.rules.result()) }.toMap)

}
