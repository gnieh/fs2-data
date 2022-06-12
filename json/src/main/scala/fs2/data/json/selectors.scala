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

package fs2
package data
package json

import cats.Show
import cats.implicits._

/** Used to select tokens in a token stream.
  */
sealed trait Selector
object Selector {

  /** Selects the current value. */
  case object ThisSelector extends Selector

  /** Selects the value in the object for which the key respects the predicate.
    * If the currently pointed value is not an object and `strict` is `true`,
    * then an error is raised, otherwise the value is skipped.
    * If the currently pointed value is an object and `mandatory` is `true`,
    * then an error is raised if the object doesn't contain all the names.
    */
  case class NameSelector(pred: NamePredicate, strict: Boolean, mandatory: Boolean) extends Selector
  object NameSelector {
    def apply(name: String, strict: Boolean, mandatory: Boolean) =
      new NameSelector(NamePredicate.Single(name), strict, mandatory)
    def apply(names: Set[String], strict: Boolean, mandatory: Boolean) =
      if (names.size <= 1) {
        names.headOption match {
          case Some(name) => new NameSelector(NamePredicate.Single(name), strict, mandatory)
          case None       => new NameSelector(NamePredicate.None, strict, mandatory)
        }
      } else {
        new NameSelector(NamePredicate.Several(names), strict, mandatory)
      }
  }

  /** Selects the value in the array for which the index respects the predicate.
    * If the currently pointed value is not an array and `strict` is `true`,
    * then an error is raised, otherwise the value is skipped.
    */
  case class IndexSelector(pred: IndexPredicate, strict: Boolean) extends Selector
  object IndexSelector {
    def apply(idx: Int, strict: Boolean) =
      new IndexSelector(IndexPredicate.Single(idx), strict)
    def apply(indices: Set[Int], strict: Boolean) =
      if (indices.size <= 1) {
        indices.headOption match {
          case Some(idx) => new IndexSelector(IndexPredicate.Single(idx), strict)
          case None      => new IndexSelector(IndexPredicate.None, strict)
        }
      } else {
        new IndexSelector(IndexPredicate.Several(indices), strict)
      }
    def apply(start: Int, end: Int, strict: Boolean) =
      if (start == end)
        new IndexSelector(IndexPredicate.Single(start), strict)
      else if (start > end)
        new IndexSelector(IndexPredicate.None, strict)
      else
        new IndexSelector(IndexPredicate.Range(start, end), strict)
  }

  /** Selects all values in an array or object.
    * If the currently pointed value is neither an array, nor an object and
    * `strict` is `true`, then an error is raised, otherwise the value is skipped.
    */
  case class IteratorSelector(strict: Boolean) extends Selector

  /** Selects whatever `left` selects on the currently pointed value
    * and then selects `right` on the result.
    * Values selected by `left` are not emitted downstream,
    * only the values selected by `right` are.
    */
  case class PipeSelector(left: Selector, right: Selector) extends Selector

  object PipeSelector {
    def from(left: Selector, right: Selector): Selector =
      (left, right) match {
        case (ThisSelector, _) => right
        case (_, ThisSelector) => left
        case (_, _)            => PipeSelector(left, right)
      }
  }

  implicit lazy val SelectorShow: Show[Selector] = Show.show {
    case ThisSelector => "."
    case NameSelector(pred, strict, mandatory) =>
      show"$pred${if (strict) "" else "?"}${if (mandatory) "!" else ""}"
    case IndexSelector(pred, strict) => show"$pred${if (strict) "" else "?"}"
    case IteratorSelector(strict)    => show".[]${if (strict) "" else "?"}"
    case PipeSelector(left, right)   => show"$left$right"
  }

}

sealed trait NamePredicate extends (String => Boolean) {

  /** Returns the set of values this predicate selects
    * if this is a finite list. Returns `Set.empty` otherwise.
    */
  def values: Set[String]
}
object NamePredicate {
  case object All extends NamePredicate {
    def apply(name: String): Boolean = true
    def values: Set[String] = Set.empty
  }
  case object None extends NamePredicate {
    def apply(name: String): Boolean = false
    def values: Set[String] = Set.empty
  }
  case class Single(name: String) extends NamePredicate {
    def apply(n: String): Boolean = n == name
    def values: Set[String] = Set(name)
  }
  case class Several(names: Set[String]) extends NamePredicate {
    def apply(name: String): Boolean = names.contains(name)
    def values: Set[String] = names
  }

  implicit lazy val NamePredicateShow: Show[NamePredicate] = Show.show {
    case All            => ".[]"
    case None           => ".[<none>]"
    case Single(name)   => s""".["$name"]"""
    case Several(names) => s""".[${names.map(n => s""""$n"""").mkString(",")}]"""
  }

}

sealed trait IndexPredicate extends (Int => Boolean)
object IndexPredicate {
  case object All extends IndexPredicate {
    def apply(idx: Int): Boolean = true
  }
  case object None extends IndexPredicate {
    def apply(idx: Int): Boolean = false
  }
  case class Single(idx: Int) extends IndexPredicate {
    def apply(n: Int): Boolean = n == idx
  }
  case class Several(indices: Set[Int]) extends IndexPredicate {
    def apply(idx: Int): Boolean = indices.contains(idx)
  }
  case class Range(start: Int, end: Int) extends IndexPredicate {
    def apply(idx: Int): Boolean = start <= idx && idx <= end
  }

  implicit lazy val IndexPredicateShow: Show[IndexPredicate] = Show.show {
    case All               => "[]"
    case None              => ".[<none>]"
    case Single(idx)       => s".[$idx]"
    case Several(indices)  => s".[${indices.mkString(",")}]"
    case Range(start, end) => s".[$start:$end]"
  }
}
