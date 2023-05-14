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

package fs2.data.esp

import cats.Show
import cats.syntax.all._

/** This is thre right-hand side of an ESP rule processor.
  * It can access the context, meaning:
  *  - the rule parameters
  *  - the current state depth
  *  - the captured inputs
  */
sealed trait Rhs[+OutTag]
object Rhs {

  /** Calls a new rule at a given depth with the given parameters. */
  case class Call[Tag](q: Int, depth: Depth, params: List[Rhs[Tag]]) extends Rhs[Tag]

  /** Calls a new rule at a given depth with the given parameters on the current input. */
  case class SelfCall[Tag](q: Int, params: List[Rhs[Tag]]) extends Rhs[Tag]

  /** Reads the rule parameter. */
  case class Param(n: Int) extends Rhs[Nothing]

  /** Empty RHS. */
  case object Epsilon extends Rhs[Nothing]

  /** Builds a tree. */
  case class Tree[OutTag](tag: OutTag, inner: Rhs[OutTag]) extends Rhs[OutTag]

  /** Builds a tree with the captured node tag in pattern. */
  case class CapturedTree[OutTag](inner: Rhs[OutTag]) extends Rhs[OutTag]

  /** Emits a leaf value. */
  case class Leaf[OutTag](value: OutTag) extends Rhs[OutTag]

  /** Emits the captured input value. */
  case object CapturedLeaf extends Rhs[Nothing]

  /** Applies the function to a leaf value. */
  case class ApplyToLeaf[OutTag](f: OutTag => Either[String, OutTag]) extends Rhs[OutTag]

  /** Concatenates two RHS. */
  case class Concat[OutTag](fst: Rhs[OutTag], snd: Rhs[OutTag]) extends Rhs[OutTag]

  def epsilon[OutTag]: Rhs[OutTag] = Epsilon

  implicit def show[O: Show]: Show[Rhs[O]] = _ match {
    case Call(q, d, Nil)     => show"q$q[$d]()"
    case Call(q, d, params)  => show"q$q[$d](${params.mkString_(", ")})"
    case SelfCall(q, Nil)    => show"q$q[0](x0)"
    case SelfCall(q, params) => show"q$q[0](x0, ${params.mkString_(", ")})"
    case Param(n)            => show"y$n"
    case Tree(tag, inner)    => show"<$tag> { $inner }"
    case CapturedTree(inner) => show"<%> { $inner }"
    case Leaf(value)         => show"$value"
    case CapturedLeaf        => "%"
    case ApplyToLeaf(_)      => "$f(%)"
    case Concat(fst, snd)    => show"$fst $snd"
    case Epsilon             => "ε"
  }

}

sealed trait Depth {
  def apply(d: Int): Int =
    this match {
      case Depth.Value(d)  => d
      case Depth.Copy      => d
      case Depth.Increment => d + 1
      case Depth.Decrement => d - 1
    }
}
object Depth {
  case class Value(d: Int) extends Depth
  case object Copy extends Depth
  case object Increment extends Depth
  case object Decrement extends Depth

  implicit val show: Show[Depth] = _ match {
    case Value(d)  => d.toString()
    case Copy      => "$d"
    case Increment => "$d + 1"
    case Decrement => "$d - 1"
  }
}
