package fs2.data.csv.generic

import fs2.data.csv.{CsvRowDecoder, Exported, RowDecoder}

import scala.reflect.macros.blackbox

/**
 * Macros used to circumvent divergence checker restrictions in the compiler. Inspired by pureconfig and circe.
 */
class ExportMacros(val c: blackbox.Context) {
  import c.universe._

  final def exportRowDecoder[A](implicit a: c.WeakTypeTag[A]): c.Expr[Exported[RowDecoder[A]]] = {
    c.typecheck(q"_root_.shapeless.lazily[_root_.fs2.data.csv.generic.DerivedRowDecoder[$a]]", silent = true) match {
      case EmptyTree => c.abort(c.enclosingPosition, s"Unable to infer value of type $a")
      case t =>
        c.Expr[Exported[RowDecoder[A]]](
          q"new _root_.fs2.data.csv.Exported($t: _root_.fs2.data.csv.RowDecoder[$a])")
    }
  }

  final def exportCsvRowDecoder[A](implicit a: c.WeakTypeTag[A]): c.Expr[Exported[CsvRowDecoder[A, String]]] = {
    c.typecheck(q"_root_.shapeless.lazily[_root_.fs2.data.csv.generic.DerivedCsvRowDecoder[$a]]", silent = false) match {
      case EmptyTree => c.abort(c.enclosingPosition, s"Unable to infer value of type $a")
      case t =>
        c.Expr[Exported[CsvRowDecoder[A, String]]](
          q"new _root_.fs2.data.csv.Exported($t: _root_.fs2.data.csv.CsvRowDecoder[$a, ${weakTypeTag[String]}])")
    }
  }
}
