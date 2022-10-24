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

package fs2.data.csv.generic.internal

import fs2.data.csv.generic.CsvName
import fs2.data.csv.{CsvRow, CsvRowDecoder, DecoderResult}
import shapeless._

trait DerivedCsvRowDecoder[T] extends CsvRowDecoder[T, String]

object DerivedCsvRowDecoder {

  final implicit def productReader[T, Repr <: HList, DefaultRepr <: HList, AnnoRepr <: HList](implicit
      gen: LabelledGeneric.Aux[T, Repr],
      defaults: Default.AsOptions.Aux[T, DefaultRepr],
      annotations: Annotations.Aux[CsvName, T, AnnoRepr],
      cc: Lazy[MapShapedCsvRowDecoder.WithDefaults[Repr, DefaultRepr, AnnoRepr]]): DerivedCsvRowDecoder[T] =
    new DerivedCsvRowDecoder[T] {
      def apply(row: CsvRow[String]): DecoderResult[T] =
        cc.value.fromWithDefault(row, defaults(), annotations()).map(gen.from(_))
    }

}
