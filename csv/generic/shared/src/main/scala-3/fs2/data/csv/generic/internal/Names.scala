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

package fs2.data.csv.generic
package internal

import shapeless3.deriving.*

// NOTE: `Names` and its given must stay publicly accessible. `deriveCsvRowDecoder`
// and `deriveCsvRowEncoder` are public and summon `Names[T]` at the *call site*, which
// lives outside this package. A `private[generic]` given is resolved there only through
// a deprecated fallback (Implicits.warnIfImplicitFromInaccessibleCompanion); since Scala
// 3.7 the compiler warns, and from Scala 3.10 it will no longer find it at all, breaking
// derivation for every downstream user. See https://github.com/gnieh/fs2-data/issues/779
trait Names[T] {
  def names: List[String]
}

object Names {
  given [T](using labels: Labelling[T], annotations: Annotations[CsvName, T]): Names[T] = new Names[T] {
    override def names: List[String] = {
      val annos = annotations.apply().toList.asInstanceOf[List[Option[CsvName]]]
      val names = labels.elemLabels.toList
      annos.zip(names).map(_.map(_.name).getOrElse(_))
    }
  }
}
