package fs2.data.csv.generic

import scala.quoted.*
import scala.compiletime.erasedValue

private[generic] object defaults {
  inline def apply[T]: List[(String, Option[Any])] = ${ defaultValuesImpl[T] }

  def defaultValuesImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[List[(String, Option[Any])]] = {
    import qctx.reflect._
    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol
    println(tpe.typeSymbol.caseFields.map(_.tree))
    val constr = symbol.primaryConstructor.tree.asInstanceOf[DefDef]
    Expr.ofList(
      constr
        .termParamss(0)
        .params
        .map {
          case v@ValDef(name, mid, rhs) =>
            println(constr)
            println(rhs)
            Expr("name" -> None)
        }
    )
  }
}
