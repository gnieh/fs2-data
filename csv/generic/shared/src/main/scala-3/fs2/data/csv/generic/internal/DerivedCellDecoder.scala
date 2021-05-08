package fs2.data.csv
package generic
package internal

import cats.syntax.all._
import shapeless3.deriving._

import scala.compiletime._
import scala.deriving.Mirror

private[generic] trait DerivedCellDecoder[T] extends CellDecoder[T]

private[generic] object DerivedCellDecoder {
  def expect[T](e: String, r: T): DerivedCellDecoder[T] = (in: String) =>
    Either.cond(in == e, r, new DecoderError(s"Expected $e, got $in"))

  given deriveUnaryProduct[T <: Product](using m: Mirror.ProductOf[T] { type MirroredElemTypes <: Any *: EmptyTuple }, ce: CellDecoder[Tuple.Head[m.MirroredElemTypes]]): DerivedCellDecoder[T] = {
    (in: String) => ce(in).map(e => m.fromProduct(Tuple1(e)))
  }

  inline given deriveCoproduct[T](using m: Mirror.SumOf[T]): DerivedCellDecoder[T] = {
    val decoders: List[DerivedCellDecoder[T]] = summonAsArray[K0.LiftP[DerivedCellDecoder, m.MirroredElemTypes]].toList.asInstanceOf
    (in: String) => decoders.foldRight(new DecoderError("Didn't match any value").asLeft)(_.apply(in).orElse(_))
  }

  inline given deriveSingleton[T](using m: Mirror.ProductOf[T] { type MirroredElemTypes = EmptyTuple }): DerivedCellDecoder[T] = summonFrom {
    //case cd: CellDecoder[T] => (in: String) => cd(in)
    case a: Annotation[CsvValue, T] => expect(a().value, m.fromProduct(EmptyTuple))
    case _ => expect(constValue[m.MirroredLabel], m.fromProduct(EmptyTuple))
  }

}


