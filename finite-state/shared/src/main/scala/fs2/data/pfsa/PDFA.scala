/*
 * Copyright 2019-2022 Lucas Satabin
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

package fs2.data.pfsa

import Pred.syntax._

private[data] class PDFA[P, T](val init: Int, val finals: Set[Int], val transitions: Array[List[(P, Int)]])(implicit
    P: Pred[P, T]) {

  def step(q: Int, t: T): Option[Int] =
    if (q >= transitions.length)
      None
    else
      transitions(q).collectFirst { case (p, q) if p.satisfies(t) => q }

}
