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

package fs2.data.automaton

import Pred.syntax._

import cats.Show
import cats.syntax.all._

class PDFA[P, T](val init: Int, val finals: Set[Int], val transitions: Map[Int, List[(P, Int)]])(implicit
    P: Pred[P, T]) {

  def step(q: Int, t: T): Option[Int] =
    transitions.get(q).flatMap(_.collectFirst { case (p, q) if p.satisfies(t) => q })

}

object PDFA {

  implicit def show[P: Show, T]: Show[PDFA[P, T]] = new Show[PDFA[P, T]] {

    implicit val transShow: Show[(P, Int)] = Show.show { case (p, q) =>
      show"q$q: $p"
    }

    def showTransitions(t: PDFA[P, T], q: Int): String =
      t.transitions
        .get(q)
        .map { ts =>
          show"q$q --> ${ts.mkString_(show"\n  q$q --> ")}"
        }
        .getOrElse("")

    override def show(t: PDFA[P, T]): String =
      show"""stateDiagram-v2
            |  direction LR
            |  [*] --> q${t.init}
            |  ${t.finals.toList.map(q => show"q$q --> [*]").mkString_("\n  ")}
            |  ${t.transitions.keySet.toList.map(showTransitions(t, _)).mkString_("\n  ")}""".stripMargin

  }

}
