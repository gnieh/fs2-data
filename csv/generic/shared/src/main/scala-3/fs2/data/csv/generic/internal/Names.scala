/*
 * Copyright 2023 Lucas Satabin
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

package fs2.data.csv.generic
package internal

import shapeless3.deriving.*

private[generic] trait Names[T] {
  def names: List[String]
}

private[generic] object Names {
  given [T](using labels: Labelling[T], annotations: Annotations[CsvName, T]): Names[T] = new Names[T] {
    override def names: List[String] = {
      val annos = annotations.apply().toList.asInstanceOf[List[Option[CsvName]]]
      val names = labels.elemLabels.toList
      annos.zip(names).map(_.map(_.name).getOrElse(_))
    }
  }
}
