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

package fs2.data.csv
package generic
package internal

import shapeless3.deriving._

import scala.deriving.Mirror

trait DerivedCellEncoder[T] extends CellEncoder[T]

object DerivedCellEncoder {
  given deriveUnaryProduct[T <: Product](using
      m: Mirror.ProductOf[T] { type MirroredElemTypes <: Any *: EmptyTuple },
      ce: CellEncoder[Tuple.Head[m.MirroredElemTypes]]): DerivedCellEncoder[T] = { (t: T) =>
    ce(t.productElement(0).asInstanceOf[Tuple.Head[m.MirroredElemTypes]])
  }

  inline given deriveCoproduct[T](using g: K0.CoproductInstances[DerivedCellEncoder, T]): DerivedCellEncoder[T] =
    new DerivedCellEncoder[T] {
      def apply(elem: T) = g.fold(elem)([t <: T] => (dce: DerivedCellEncoder[t], te: t) => dce(te))
    }

  inline given deriveSingleton[T](using cv: CellValue[T]): DerivedCellEncoder[T] =
    new DerivedCellEncoder[T] {
      def apply(t: T) = cv.value
    }

}
