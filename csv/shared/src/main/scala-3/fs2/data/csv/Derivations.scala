package fs2.data.csv

import cats.data.NonEmptyList

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

object Derivations {

  private def mySummonAll[T <: Tuple: Type](using Quotes): List[Expr[CellEncoder[_]]] =
    Type.of[T] match {
      case '[tpe *: tpes] => '{ summonInline[CellEncoder[tpe]] } :: mySummonAll[tpes]
      case '[EmptyTuple] => Nil
    }

  private def mySummonAllDecoders[T <: Tuple: Type](using Quotes): List[Expr[CellDecoder[_]]] =
    Type.of[T] match {
      case '[tpe *: tpes] => '{ summonInline[CellDecoder[tpe]] } :: mySummonAllDecoders[tpes]
      case '[EmptyTuple] => Nil
    }

  private def summonLabelsRec[T <: Tuple: Type](using Quotes): List[Expr[String]] = Type.of[T] match {
    case '[tpe *: tpes] => '{ summonInline[tpe].asInstanceOf[String] } :: summonLabelsRec[tpes]
    case '[EmptyTuple] => Nil
  }

  def deriveRowEncoder[T: Type](using q1: Quotes): Expr[RowEncoder[T]] = {
    import quotes.reflect.*

    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    val tpe = TypeRepr.of[T]

    ev match {
      case '{ type elemTypes <: Tuple; $m: Mirror.ProductOf[T] { type MirroredElemTypes = `elemTypes` } } =>
        val elemCellEncoders: List[Expr[CellEncoder[_]]] = mySummonAll[elemTypes]
        val body: (Expr[T]) => Expr[List[String]] = (t) => {
          elemCellEncoders.zipWithIndex.foldRight('{List.empty[String]}) { case ((encoder, index), acc) =>
            '{ $encoder.asInstanceOf[CellEncoder[Any]].apply($t.asInstanceOf[Product].productElement(${Expr(index)})) :: ${acc} }
          }
        }

        '{ RowEncoder.instance((t: T) => NonEmptyList.fromListUnsafe(${ body('t) })) }
    }
  }

  def searchImplicits[T <: Tuple: Type](using Quotes): List[Expr[CellEncoder[_]]] =
    Type.of[T] match {
      case '[tpe *: tpes] => '{ summonInline[CellEncoder[tpe]] } :: mySummonAll[tpes]
      case '[EmptyTuple] => Nil
    }

  def deriveCsvRowDecoder[T: Type](using q1: Quotes): Expr[CsvRowDecoder[T, String]] = {
    import quotes.reflect.*

    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    val tpe = TypeRepr.of[T]

    def extractName(e: Term): Option[Expr[String]] = Some(e.asExpr).collectFirst {
      case '{ new CsvName($name) } => name
    }

    ev match {
      case '{ type elemTypes <: Tuple; type elemLabels <: NonEmptyTuple; $m: Mirror.ProductOf[T] { type MirroredElemTypes = `elemTypes`; type MirroredElemLabels = `elemLabels` } } =>
        val elemCellDecoders: List[Expr[CellDecoder[_]]] = mySummonAllDecoders[elemTypes]
        //val elems = tpe.typeSymbol.caseFields //summonLabelsRec[elemLabels]
        val elemLabels = tpe.typeSymbol.caseFields.map(_.name) //summonLabelsRec[elemLabels]
        val body: Expr[CsvRow[String]] => (Expr[Either[fs2.data.csv.DecoderError, ? <: Tuple]]) = (row) => {
        elemCellDecoders.zipWithIndex.foldRight('{Right(EmptyTuple): Either[DecoderError, _ <: Tuple]}) { case ((decoder, index), acc) =>
            val name = elemLabels.apply(index)
            val resolvedName = tpe.typeSymbol.primaryConstructor.paramSymss(0)(index).getAnnotation(Symbol.requiredClass("fs2.data.csv.CsvName")).flatMap(extractName).getOrElse(Expr(name))
            //val decoders: List[Expr[_]] = searchImplicits(TypeRepr.of[Tuple.Map[elemTypes, CellDecoder]])
            //'{ ${acc}.flatMap(a => $decoder.asInstanceOf[CellDecoder[Any]].apply($resolvedName).map(_ *: a)) }
            '{ ${acc}.flatMap(a => $decoder.asInstanceOf[CellDecoder[Any]].apply($row($resolvedName).get).map(_ *: a)) }
          }
        }

        '{ CsvRowDecoder.instance((row: CsvRow[String]) => ${ body('row) }.map($m.fromProduct)) }
    }
  }

}
