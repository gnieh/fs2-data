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

import cats._
import cats.data._
import cats.syntax.all._
import shapeless3.deriving._
import scala.compiletime._

object semiauto {

  // shapeless3 <-> cats interop
  private given [F[_]: Applicative]: Pure[F] = [t] => (a: t) => Applicative[F].pure(a)
  private given [F[_]: Functor]: MapF[F] = [t, b] => (ft: F[t], f: t => b) => Functor[F].map(ft)(f)
  private given [F[_]: FlatMap]: TailRecM[F] = [t, b] => (a: t, f: t => F[Either[t, b]]) => FlatMap[F].tailRecM(a)(f)

  def deriveRowDecoder[T](using ic: K0.ProductInstances[OptCellDecoder, T]): RowDecoder[T] = {
    new RowDecoder[T] {
      override def apply(row: Row): DecoderResult[T] = {
        ic.constructM[StateT[DecoderResult, List[(String, Int)], *]] {
          [t] =>
            (cd: OptCellDecoder[t]) =>
              StateT[DecoderResult, List[(String, Int)], t] {
                case (hd, idx) :: tail => cd(s"at index $idx", Some(hd)).tupleLeft(tail)
                // do not fail immediately as optional decoding could succeed without input
                case Nil => cd(s"at index -1", None).tupleLeft(Nil)
            }
        }(summon, summon, summon)
          .runA(row.values.toList.zipWithIndex)
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
      naming: Names[T]): CsvRowDecoder[T, String] = {
    val names: List[String] = naming.names
    new CsvRowDecoder[T, String] {
      override def apply(row: CsvRow[String]): DecoderResult[T] = {
        ic.constructM[StateT[DecoderResult, List[(String, Option[String])], *]] {
          [t] =>
            (cd: OptCellDecoder[t]) =>
              StateT[DecoderResult, List[(String, Option[String])], t] {
                case (name, value) :: tail => cd(name, value).tupleLeft(tail)
                case Nil                   => new DecoderError("Bug in derivation logic.").asLeft
            }
        }(summon, summon, summon)
          .runA(names.map(name => name -> row.toMap.get(name)))
      }
    }
  }

  def deriveCsvRowEncoder[T](using ic: K0.ProductInstances[CellEncoder, T], naming: Names[T]) =
    new CsvRowEncoder[T, String] {
      val names: List[String] = naming.names
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

  inline def deriveCellDecoder[T]: CellDecoder[T] = summonInline[DerivedCellDecoder[T]]

  inline def deriveCellEncoder[T]: CellEncoder[T] = summonInline[DerivedCellEncoder[T]]

  // bincompat stubs

  private[generic] def deriveCsvRowDecoder[T](ic: K0.ProductInstances[OptCellDecoder, T],
                                              labels: Labelling[T],
                                              annotations: Annotations[CsvName, T]): CsvRowDecoder[T, String] = {
    given Labelling[T] = labels
    given Annotations[CsvName, T] = annotations
    deriveCsvRowDecoder[T](using ic = ic, naming = summon[Names[T]])
  }

  private[generic] def deriveCsvRowEncoder[T](ic: K0.ProductInstances[CellEncoder, T],
                                              labels: Labelling[T],
                                              annotations: Annotations[CsvName, T]): CsvRowEncoder[T, String] = {
    given Labelling[T] = labels
    given Annotations[CsvName, T] = annotations
    deriveCsvRowEncoder[T](using ic = ic, naming = summon[Names[T]])
  }

}
