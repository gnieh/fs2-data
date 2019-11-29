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