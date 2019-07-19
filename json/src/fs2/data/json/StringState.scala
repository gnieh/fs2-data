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
package fs2.data.json

import scala.annotation.switch

private sealed trait StringState

private object StringState {
  case object Normal extends StringState
  case object SeenBackslash extends StringState

  sealed abstract class ExpectUnicode(val n: Int) extends StringState
  object ExpectUnicode {
    def apply(n: Int): ExpectUnicode =
      (n: @switch) match {
        case 1 => Expect1Unicode
        case 2 => Expect2Unicode
        case 3 => Expect3Unicode
      }
    def unapply(eu: ExpectUnicode): Option[Int] =
      Some(eu.n)
  }
  case object Expect4Unicode extends ExpectUnicode(4)
  case object Expect3Unicode extends ExpectUnicode(3)
  case object Expect2Unicode extends ExpectUnicode(2)
  case object Expect1Unicode extends ExpectUnicode(1)
}
