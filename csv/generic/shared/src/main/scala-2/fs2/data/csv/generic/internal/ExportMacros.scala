/*
 * Copyright 2024 fs2-data Project
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

package fs2.data.csv.generic.internal

import fs2.data.csv._

import scala.reflect.macros.blackbox

/** Macros used to circumvent divergence checker restrictions in the compiler. Inspired by pureconfig and circe.
  */
class ExportMacros(val c: blackbox.Context) {
  import c.universe._

  final def exportRowDecoder[A](implicit a: c.WeakTypeTag[A]): c.Expr[Exported[RowDecoder[A]]] = {
    c.typecheck(q"_root_.shapeless.lazily[_root_.fs2.data.csv.generic.internal.DerivedRowDecoder[$a]]",
                silent = true) match {
      case EmptyTree =>
        c.abort(c.enclosingPosition, s"Unable to infer value of type $a")
      case t =>
        c.Expr[Exported[RowDecoder[A]]](q"new _root_.fs2.data.csv.Exported($t: _root_.fs2.data.csv.RowDecoder[$a])")
    }
  }

  final def exportRowEncoder[A](implicit a: c.WeakTypeTag[A]): c.Expr[Exported[RowEncoder[A]]] = {
    c.typecheck(q"_root_.shapeless.lazily[_root_.fs2.data.csv.generic.internal.DerivedRowEncoder[$a]]",
                silent = true) match {
      case EmptyTree =>
        c.abort(c.enclosingPosition, s"Unable to infer value of type $a")
      case t =>
        c.Expr[Exported[RowEncoder[A]]](q"new _root_.fs2.data.csv.Exported($t: _root_.fs2.data.csv.RowEncoder[$a])")
    }
  }

  final def exportCsvRowDecoder[A](implicit a: c.WeakTypeTag[A]): c.Expr[Exported[CsvRowDecoder[A, String]]] = {
    c.typecheck(q"_root_.shapeless.lazily[_root_.fs2.data.csv.generic.internal.DerivedCsvRowDecoder[$a]]",
                silent = true) match {
      case EmptyTree =>
        c.abort(c.enclosingPosition, s"Unable to infer value of type $a")
      case t =>
        c.Expr[Exported[CsvRowDecoder[A, String]]](
          q"new _root_.fs2.data.csv.Exported($t: _root_.fs2.data.csv.CsvRowDecoder[$a, ${weakTypeTag[String]}])")
    }
  }

  final def exportCsvRowEncoder[A](implicit a: c.WeakTypeTag[A]): c.Expr[Exported[CsvRowEncoder[A, String]]] = {
    c.typecheck(q"_root_.shapeless.lazily[_root_.fs2.data.csv.generic.internal.DerivedCsvRowEncoder[$a]]",
                silent = true) match {
      case EmptyTree =>
        c.abort(c.enclosingPosition, s"Unable to infer value of type $a")
      case t =>
        c.Expr[Exported[CsvRowEncoder[A, String]]](
          q"new _root_.fs2.data.csv.Exported($t: _root_.fs2.data.csv.CsvRowEncoder[$a, ${weakTypeTag[String]}])")
    }
  }

  final def exportCellDecoder[A](implicit a: c.WeakTypeTag[A]): c.Expr[Exported[CellDecoder[A]]] = {
    c.typecheck(q"_root_.shapeless.lazily[_root_.fs2.data.csv.generic.internal.DerivedCellDecoder[$a]]",
                silent = true) match {
      case EmptyTree =>
        c.abort(c.enclosingPosition, s"Unable to infer value of type $a")
      case t =>
        c.Expr[Exported[CellDecoder[A]]](q"new _root_.fs2.data.csv.Exported($t: _root_.fs2.data.csv.CellDecoder[$a])")
    }
  }

  final def exportCellEncoder[A](implicit a: c.WeakTypeTag[A]): c.Expr[Exported[CellEncoder[A]]] = {
    c.typecheck(q"_root_.shapeless.lazily[_root_.fs2.data.csv.generic.internal.DerivedCellEncoder[$a]]",
                silent = true) match {
      case EmptyTree =>
        c.abort(c.enclosingPosition, s"Unable to infer value of type $a")
      case t =>
        c.Expr[Exported[CellEncoder[A]]](q"new _root_.fs2.data.csv.Exported($t: _root_.fs2.data.csv.CellEncoder[$a])")
    }
  }
}
