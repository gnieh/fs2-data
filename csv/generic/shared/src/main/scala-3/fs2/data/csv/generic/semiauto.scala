/*
 * Copyright 2022 Lucas Satabin
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

package fs2
package data
package csv
package generic

import internal._

import cats.syntax.all._
import shapeless3.deriving._
import scala.deriving._
import scala.compiletime._
import scala.quoted._

import scala.Tuple.Union

object semiauto {

  def deriveRowDecoder[T](using ic: K0.ProductInstances[OptCellDecoder, T]): RowDecoder[T] = {
    new RowDecoder[T] {
      override def apply(row: Row): DecoderResult[T] = {
        val ((_, _, errorOpt), decodedOpt) =
          ic.unfold[(Int, List[String], Option[DecoderError])]((0, row.values.toList, None))(
            [t] =>
              (acc: (Int, List[String], Option[DecoderError]), cd: OptCellDecoder[t]) => {
                val result = cd(s"at index ${acc._1}", acc._2.headOption.filter(_.nonEmpty))
                ((acc._1 + 1, acc._2.tail, result.left.toOption), result.toOption)
            }
          )
        decodedOpt.toRight(errorOpt.getOrElse(new DecoderError("Bug in derivation logic.")))
      }
    }
  }

  def deriveRowEncoder[T](using ic: K0.ProductInstances[CellEncoder, T]): RowEncoder[T] = new RowEncoder[T] {
    override def apply(elem: T): Row = Row(
      cats.data.NonEmptyList
        .fromListUnsafe(
          ic.foldLeft(elem)(List.empty[String])(
            [t] => (acc: List[String], ce: CellEncoder[t], e: t) => Continue[List[String]](ce(e) :: acc)
          ))
        .reverse)
  }

  def deriveCsvRowDecoder[T](using
      ic: K0.ProductInstances[OptCellDecoder, T],
      labels: Labelling[T],
      annotations: Annotations[CsvName, T]): CsvRowDecoder[T, String] = {
    val names: List[String] = namesFor[T]
    new CsvRowDecoder[T, String] {
      override def apply(row: CsvRow[String]): DecoderResult[T] = {
        val ((_, _, errorOpt), decodedOpt) = ic.unfold[(Int, CsvRow[String], Option[DecoderError])]((0, row, None))(
          [t] =>
            (acc: (Int, CsvRow[String], Option[DecoderError]), cd: OptCellDecoder[t]) => {
              val columnName = names(acc._1)
              val result = cd.apply(columnName, acc._2(columnName).filter(_.nonEmpty))
              ((acc._1 + 1, row, result.left.toOption), result.toOption)
          }
        )
        decodedOpt.toRight(errorOpt.getOrElse(new DecoderError("Bug in derivation logic.")))
      }
    }
  }

  def deriveCsvRowEncoder[T](using
      ic: K0.ProductInstances[CellEncoder, T],
      labels: Labelling[T],
      annotations: Annotations[CsvName, T]) = new CsvRowEncoder[T, String] {
    val names: List[String] = namesFor[T]
    type Acc = (List[String], List[(String, String)])
    override def apply(elem: T): CsvRow[String] = {
      val columns = ic
        .foldLeft[Acc](elem)(names -> List.empty[(String, String)])(
          [t] => (acc: Acc, ce: CellEncoder[t], e: t) => Continue((acc._1.tail, ((acc._1.head -> ce(e)) :: acc._2)))
        )
        ._2
      CsvRow.fromNelHeaders(cats.data.NonEmptyList.fromListUnsafe(columns.reverse))
    }
  }

  private def namesFor[T](using labels: Labelling[T], annotations: Annotations[CsvName, T]): List[String] = {
    val annos = annotations.apply().toList.asInstanceOf[List[Option[CsvName]]]
    val names = labels.elemLabels.toList
    annos.zip(names).map(_.map(_.name).getOrElse(_))
  }

  inline def deriveCellDecoder[T]: CellDecoder[T] = summonInline[DerivedCellDecoder[T]]

  inline def deriveCellEncoder[T]: CellEncoder[T] = summonInline[DerivedCellEncoder[T]]

}
