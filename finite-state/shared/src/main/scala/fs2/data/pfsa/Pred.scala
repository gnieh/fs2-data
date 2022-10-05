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

package fs2.data.pfsa

/** A typeclass for predicates, that can be combined together. */
trait Pred[P, Elt] {

  /** Whether the element `e` satisfies the predicate. */
  def satsifies(p: P)(e: Elt): Boolean

  /** The predicate that is always true. */
  def always: P

  /** The predicate that is always false. */
  def never: P

  /** Conjunction of both predicates. */
  def and(p1: P, p2: P): P

  /** Disjunction of both predicates. */
  def or(p1: P, p2: P): P

  /** The negation of this predicate. */
  def not(p: P): P

  /** Whether the predicate is not obviously non satisfiable. */
  def isSatisfiable(p: P): Boolean

}

object Pred {

  def apply[P, T](implicit ev: Pred[P, T]): Pred[P, T] = ev

  object syntax {
    implicit class PredOps[P](val p1: P) extends AnyVal {
      def satisfies[Elt](e: Elt)(implicit P: Pred[P, Elt]): Boolean =
        P.satsifies(p1)(e)

      def &&[Elt](p2: P)(implicit P: Pred[P, Elt]): P =
        P.and(p1, p2)

      def ||[Elt](p2: P)(implicit P: Pred[P, Elt]): P =
        P.or(p1, p2)

      def unary_![Elt](implicit P: Pred[P, Elt]): P =
        P.not(p1)

      def isSatisfiable[Elt](implicit P: Pred[P, Elt]): Boolean =
        P.isSatisfiable(p1)
    }
  }

}
