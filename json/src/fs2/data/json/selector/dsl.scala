/*
 * Copyright 2020 Lucas Satabin
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
package selector

import scala.annotation.implicitNotFound

/** Selector builder exposes a DSL to build selectors in a type safe manner. */
sealed abstract class SelectorBuilder[M, S] private[selector] {

  /** Creates an iterator selector, that will enumerate all elements in the currently selected array or object.
    * The resulting selector is strict and will fail if the element is not an array or an object.
    */
  def iterate: IteratorBuilder[Strict] =
    IteratorBuilder[Strict](true, this)

  /** Creates an index selector, that will select only the element at the given index in the currently selected array.
    * The resulting selector is strict and will fail if the element is not an array.
    */
  def index(i: Int): IndicesBuilder[Strict] =
    IndicesBuilder[Strict](IndexPredicate.Single(i), true, this)

  /** Creates an indices selector, that will select only the elements at the given indices in the currently selected array.
    * The resulting selector is strict and will fail if the element is not an array.
    */
  def indices(i: Int, is: Int*): IndicesBuilder[Strict] =
    IndicesBuilder[Strict](IndexPredicate.Several(is.toSet + i), true, this)

  /** Creates a range selector, that will select only the elements between the given indices in the currently selected array.
    * The resulting selector is strict and will fail if the element is not an array.
    */
  def range(start: Int, end: Int): IndicesBuilder[Strict] =
    IndicesBuilder[Strict](IndexPredicate.Range(start, end), true, this)

  /** Creates a field selector, that will select only the element at the given field in the currently selected object.
    * The resulting selector is strict and will fail if the element is not an object.
    */
  def field(f: String): NamesBuilder[Optional, Strict] =
    NamesBuilder[Optional, Strict](NamePredicate.Single(f), strict = true, mandatory = false, this)

  /** Creates a fields selector, that will select only the element at the given fields in the currently selected object.
    * The resulting selector is strict and will fail if the element is not an object.
    */
  def fields(f: String, fs: String*): NamesBuilder[Optional, Strict] =
    NamesBuilder[Optional, Strict](NamePredicate.Several(fs.toSet + f), strict = true, mandatory = false, this)

  /** Compiles the current builder into its final selector representation. */
  def compile: Selector

}

object SelectorBuilder {

  implicit class SelectorOps[M, S, B <: SelectorBuilder[M, S]](val inner: B with SelectorBuilder[M, S]) extends AnyVal {

    /** Makes the selector to which it applies mandatory.
      * The selection will fail if the selected fields are not present in the object.
      */
    def ![O](implicit M: Mandatoriable.Aux[B, O, S]): O =
      M.makeMandatory(inner)

    /** Makes the selector to which it applies lenient.
      * The selection will '''not''' fail if the value it applies to is not an array or object.
      */
    def ?[O](implicit L: Lenientable.Aux[B, O, M]): O =
      L.makeLenient(inner)

  }

}

case object RootBuilder extends SelectorBuilder[NotApplicable, NotApplicable] {
  def compile: Selector = Selector.ThisSelector
}

case class IteratorBuilder[S](strict: Boolean, parent: SelectorBuilder[_, _])
    extends SelectorBuilder[NotApplicable, S] {
  def compile: Selector = Selector.PipeSelector.from(parent.compile, Selector.IteratorSelector(strict))
}

object IteratorBuilder {

  implicit def LenientableIterator: Lenientable.Aux[IteratorBuilder[Strict], IteratorBuilder[Lenient], NotApplicable] =
    new Lenientable[IteratorBuilder[Strict], NotApplicable] {
    type Out = IteratorBuilder[Lenient]
    def makeLenient(builder: IteratorBuilder[Strict]): IteratorBuilder[Lenient] =
      builder.copy(strict = false)
  }

}

case class IndicesBuilder[S](predicate: IndexPredicate, strict: Boolean, parent: SelectorBuilder[_, _])
    extends SelectorBuilder[NotApplicable, S] {
  def compile: Selector = Selector.PipeSelector.from(parent.compile, Selector.IndexSelector(predicate, strict))
}

object IndicesBuilder {

  implicit def LenientableIndices: Lenientable.Aux[IndicesBuilder[Strict], IndicesBuilder[Lenient], NotApplicable] =
    new Lenientable[IndicesBuilder[Strict], NotApplicable] {
      type Out = IndicesBuilder[Lenient]
      def makeLenient(builder: IndicesBuilder[Strict]): IndicesBuilder[Lenient] =
        builder.copy(strict = false)
    }

}

case class NamesBuilder[M, S](predicate: NamePredicate,
                              strict: Boolean,
                              mandatory: Boolean,
                              parent: SelectorBuilder[_, _])
    extends SelectorBuilder[M, S] {
  def compile: Selector =
    Selector.PipeSelector.from(parent.compile, Selector.NameSelector(predicate, strict, mandatory))
}

object NamesBuilder {

  implicit def LenientableNames[M]: Lenientable.Aux[NamesBuilder[M, Strict], NamesBuilder[M, Lenient], M] =
    new Lenientable[NamesBuilder[M, Strict], M] {
      type Out = NamesBuilder[M, Lenient]
      def makeLenient(builder: NamesBuilder[M, Strict]): NamesBuilder[M, Lenient] =
        builder.copy(strict = false)
    }

  implicit def MandatoriableNames[S]: Mandatoriable.Aux[NamesBuilder[Optional, S], NamesBuilder[Mandatory, S], S] =
    new Mandatoriable[NamesBuilder[Optional, S], S] {
      type Out = NamesBuilder[Mandatory, S]
      def makeMandatory(builder: NamesBuilder[Optional, S]): NamesBuilder[Mandatory, S] =
        builder.copy(mandatory = true)
    }

}

/** Marker class to notify that a given selector builder capability is not applicable for this case. */
class NotApplicable private {}
/** Marker class to notify that a selector '''doesn't''' require the elements it selects to be present. */
class Optional private {}
/** Marker class to notify that a selector requires the elements it selects to be present. */
class Mandatory private {}
/** Marker class to notify that a selector requires the type of the element it is applied to to be the expected one (array or object) */
class Strict private {}
/** Marker class to notify that a selector '''doesn't''' require the type of the element it is applied to to be the expected one (array or object) */
class Lenient private {}

sealed trait Lenientable[In, M] {
  type Out <: SelectorBuilder[M, Lenient]
  def makeLenient(builder: In): Out
}

object Lenientable {
  @implicitNotFound(msg = "There seems to be no way to make this selector lenient. Is it already?")
  type Aux[In, Out0, M] = Lenientable[In, M] { type Out = Out0 }
}

sealed trait Mandatoriable[In, S] {
  type Out <: SelectorBuilder[Mandatory, S]
  def makeMandatory(builder: In): Out
}

object Mandatoriable {
  @implicitNotFound(msg = "Only not yet mandatory field selectors can be made mandatory")
  type Aux[In, Out0, S] = Mandatoriable[In, S] { type Out = Out0 }
}
