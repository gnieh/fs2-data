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
package fs2.data.csv.generic

import fs2.data.csv.{CsvRowDecoder, Exported, RowDecoder}

import scala.language.experimental.macros

trait AutoDerivedRowDecoders {
  implicit def exportRowDecoder[A]: Exported[RowDecoder[A]] = macro ExportMacros.exportRowDecoder[A]
}

trait AutoDerivedCsvRowDecoders {
  implicit def exportCsvRowDecoder[A]: Exported[CsvRowDecoder[A, String]] = macro ExportMacros.exportCsvRowDecoder[A]
}

object auto extends AutoDerivedRowDecoders with AutoDerivedCsvRowDecoders {
  object row extends AutoDerivedRowDecoders
  object csvrow extends AutoDerivedCsvRowDecoders
}