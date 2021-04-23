package fs2.data.csv.generic

import scala.quoted.*

object Debug {
  inline def debugSingle(inline expr: Any): String =
    ${debugSingleImpl('expr)}

  private def debugSingleImpl(expr: Expr[Any])(using Quotes): Expr[String] =
    '{ "Value of " + ${Expr(Expr.betaReduce(expr).show)} + " is " + $expr }
}
