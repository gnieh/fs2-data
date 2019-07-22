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

private object NumberState {
  final val NumberStart = 0
  final val IntegerStart = 1
  final val IntegerBody = 2
  final val FractionStart = 3
  final val FractionOne = 4
  final val FractionBody = 5
  final val ExponentSign = 6
  final val ExponentOne = 7
  final val ExponentBody = 8
  final val Invalid = 9
  def isFinal(state: Int): Boolean =
    (state: @switch) match {
      case 2 | 3 | 5 | 8 => true
      case _             => false
    }
}
