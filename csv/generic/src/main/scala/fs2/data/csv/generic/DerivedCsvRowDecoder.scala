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
package csv
package generic

import shapeless._

trait DerivedCsvRowDecoder[T] extends CsvRowDecoder[T, String]

object DerivedCsvRowDecoder {

  final implicit def productReader[T, Repr <: HList, DefaultRepr <: HList, NamesAnno <: HList, EmbedsAnno <: HList](
      implicit
      gen: LabelledGeneric.Aux[T, Repr],
      defaults: Default.AsOptions.Aux[T, DefaultRepr],
      names: Annotations.Aux[CsvName, T, NamesAnno],
      embeds: Annotations.Aux[CsvEmbed, T, EmbedsAnno],
      cc: Lazy[MapShapedCsvRowDecoder.WithDefaults[T, Repr, DefaultRepr, NamesAnno, EmbedsAnno]])
      : DerivedCsvRowDecoder[T] =
    (row: CsvRow[String]) => cc.value.fromWithDefault(row, defaults(), names(), embeds()).map(gen.from(_))

}
