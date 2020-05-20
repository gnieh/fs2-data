/*
 * Copyright 2019 Lucas Satabin
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

import cats._

/** Describes how a row can be encoded from a value of the given type.
  */
trait CsvRowEncoder[T, Header] {
  def apply(elem: T): CsvRow[Header]
}

object CsvRowEncoder extends ExportedCsvRowEncoders {

  implicit def CsvRowEncoderContravariant[Header]: Contravariant[CsvRowEncoder[*, Header]] =
    new Contravariant[CsvRowEncoder[*, Header]] {
      override def contramap[A, B](fa: CsvRowEncoder[A, Header])(f: B => A): CsvRowEncoder[B, Header] =
        elem => fa(f(elem))
    }

  def apply[T: CsvRowEncoder[*, Header], Header]: CsvRowEncoder[T, Header] =
    implicitly[CsvRowEncoder[T, Header]]

}

trait ExportedCsvRowEncoders {
  implicit def exportedCsvRowEncoders[A](
      implicit exported: Exported[CsvRowEncoder[A, String]]): CsvRowEncoder[A, String] = exported.instance
}
