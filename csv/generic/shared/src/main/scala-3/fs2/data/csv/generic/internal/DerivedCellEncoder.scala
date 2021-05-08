package fs2.data.csv
package generic
package internal

import shapeless3.deriving._

import scala.compiletime._
import scala.deriving.Mirror

private[generic] trait DerivedCellEncoder[T] extends CellEncoder[T]

private[generic] object DerivedCellEncoder {
  given deriveUnaryProduct[T <: Product](using m: Mirror.ProductOf[T] { type MirroredElemTypes <: Any *: EmptyTuple }, ce: CellEncoder[Tuple.Head[m.MirroredElemTypes]]): DerivedCellEncoder[T] = {
    (t: T) => ce(t.productElement(0).asInstanceOf[Tuple.Head[m.MirroredElemTypes]])
  }

  inline given deriveCoproduct[T](using g: K0.CoproductInstances[DerivedCellEncoder, T]): DerivedCellEncoder[T] =
    (elem: T) => g.fold(elem)([t] => (dce: DerivedCellEncoder[t], te: t) => dce(te))

  inline given deriveSingleton[T](using m: Mirror.ProductOf[T]  { type MirroredElemTypes = EmptyTuple }): DerivedCellEncoder[T] = summonFrom {
    //case ce: CellEncoder[T] => (t: T) => ce(t)
    case a: Annotation[CsvValue, T] => (t: T) => a().value
    case _ => (t: T) => constValue[m.MirroredLabel]
  }

}
