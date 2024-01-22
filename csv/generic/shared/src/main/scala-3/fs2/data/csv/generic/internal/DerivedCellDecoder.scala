/*
 * Copyright 2024 fs2-data Project
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

import cats.syntax.all._
import shapeless3.deriving._

import scala.deriving.Mirror

trait DerivedCellDecoder[T] extends CellDecoder[T]

object DerivedCellDecoder {
  def expect[T](e: String, r: T): DerivedCellDecoder[T] = (in: String) =>
    Either.cond(in == e, r, new DecoderError(s"Expected $e, got $in"))

  given deriveUnaryProduct[T <: Product](using
      m: Mirror.ProductOf[T] { type MirroredElemTypes <: Any *: EmptyTuple },
      ce: CellDecoder[Tuple.Head[m.MirroredElemTypes]]): DerivedCellDecoder[T] = { (in: String) =>
    ce(in).map(e => m.fromProduct(Tuple1(e)))
  }

  inline given deriveCoproduct[T](using m: Mirror.SumOf[T]): DerivedCellDecoder[T] = {
    val decoders: List[DerivedCellDecoder[T]] =
      summonAsArray[K0.LiftP[DerivedCellDecoder, m.MirroredElemTypes]].toList.asInstanceOf
    new DerivedCellDecoder[T] {
      def apply(in: String) =
        decoders.foldRight(new DecoderError("Didn't match any value").asLeft)(_.apply(in).orElse(_))
    }
  }

  inline given deriveSingleton[T](using cv: CellValue[T], m: Mirror.ProductOf[T]): DerivedCellDecoder[T] =
    expect(cv.value, m.fromProduct(EmptyTuple))

}
