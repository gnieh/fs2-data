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

package fs2.data.csv
package generic.internal

import cats.syntax.all.*

import scala.util.NotGiven

sealed trait OptCellDecoder[T] {
  def apply(name: String, value: Option[String]): DecoderResult[T]
}

object OptCellDecoder extends LowPrioOptCellDecoders {
  given makeNonOpt[A: CellDecoder](using NotGiven[A <:< Option[_]]): OptCellDecoder[A] = new OptCellDecoder[A] {
    override def apply(name: String, value: Option[String]): DecoderResult[A] =
      value.toRight(new DecoderError(s"unknown column name '$name'")).flatMap(CellDecoder[A].apply)
  }

  given makeExplicitOpt[A](using cd: CellDecoder[Option[A]]): OptCellDecoder[Option[A]] =
    new OptCellDecoder[Option[A]] {
      override def apply(name: String, value: Option[String]): DecoderResult[Option[A]] =
        value.flatTraverse(cd.apply)
    }
}

trait LowPrioOptCellDecoders {
  given makeOpt[A: CellDecoder]: OptCellDecoder[Option[A]] = new OptCellDecoder[Option[A]] {
    override def apply(name: String, value: Option[String]): DecoderResult[Option[A]] =
      value.filter(_.nonEmpty).traverse(CellDecoder[A].apply(_))
  }
}
