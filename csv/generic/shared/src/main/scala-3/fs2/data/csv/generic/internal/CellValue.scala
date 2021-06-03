package fs2.data.csv.generic.internal

import fs2.data.csv.generic.CsvValue
import shapeless3.deriving.Annotation

import scala.compiletime.*
import scala.deriving.*

trait CellValue[T] {
  def value: String
}

object CellValue {
  inline given deriveSingleton[T](using m: Mirror.ProductOf[T] { type MirroredElemTypes = EmptyTuple }): CellValue[T] =
    summonFrom {
      case a: Annotation[CsvValue, T] => new CellValue[T] { def value: String = a().value }
      case _                          => new CellValue[T] { def value: String = constValue[m.MirroredLabel] }
    }
}
