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
package json

/** Used to select tokens in a token stream.
  */
sealed trait Selector
object Selector {

  /** Selects the current value. */
  case object ThisSelector extends Selector

  /** Selects the value in the object for which the key respects the predicate.
    * If the currently pointed value is not an object and `strict` is `true`,
    * then an error is raised, otherwise the value is skipped.
    */
  case class NameSelector(pred: NamePredicate, strict: Boolean) extends Selector
  object NameSelector {
    def apply(name: String, strict: Boolean) =
      new NameSelector(NamePredicate.Single(name), strict)
    def apply(names: Seq[String], strict: Boolean) =
      names match {
        case Seq(name) => new NameSelector(NamePredicate.Single(name), strict)
        case Seq()     => new NameSelector(NamePredicate.None, strict)
        case _         => new NameSelector(NamePredicate.Several(names.toSet), strict)
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
    def apply(indices: Seq[Int], strict: Boolean) =
      indices match {
        case Seq(idx) => new IndexSelector(IndexPredicate.Single(idx), strict)
        case Seq()    => new IndexSelector(IndexPredicate.None, strict)
        case _        => new IndexSelector(IndexPredicate.Several(indices.toSet), strict)
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
}

sealed trait NamePredicate extends (String => Boolean)
object NamePredicate {
  case object All extends NamePredicate {
    def apply(name: String): Boolean = true
  }
  case object None extends NamePredicate {
    def apply(name: String): Boolean = false
  }
  case class Single(name: String) extends NamePredicate {
    def apply(n: String): Boolean = n == name
  }
  case class Several(names: Set[String]) extends NamePredicate {
    def apply(name: String): Boolean = names.contains(name)
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
}
