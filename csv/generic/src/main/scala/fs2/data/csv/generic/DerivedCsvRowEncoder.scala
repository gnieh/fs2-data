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

trait DerivedCsvRowEncoder[T] extends CsvRowEncoder[T, String]

object DerivedCsvRowEncoder {

  final implicit def productWriter[T, Repr <: HList, NameAnno <: HList, EmbedAnno <: HList](implicit
      gen: LabelledGeneric.Aux[T, Repr],
      names: Annotations.Aux[CsvName, T, NameAnno],
      embeds: Annotations.Aux[CsvEmbed, T, EmbedAnno],
      cc: Lazy[MapShapedCsvRowEncoder.WithAnnotations[T, Repr, NameAnno, EmbedAnno]]): DerivedCsvRowEncoder[T] =
    (elem: T) => cc.value.fromWithAnnotation(gen.to(elem), names(), embeds())

}
