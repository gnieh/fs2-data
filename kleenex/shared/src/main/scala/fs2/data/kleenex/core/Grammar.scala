/*
 * Copyright 2021 Lucas Satabin
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

package fs2.data
package kleenex
package core

import transducer.CharRanges

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all._

case class Program(pipeline: NonEmptyList[Int], decls: Map[Int, Term])
object Program {
  implicit val show: Show[Program] = Show.show { case Program(pipeline, decls) =>
    s"""start: ${pipeline.mkString_(" >> ")}
       |
       |${decls.toList.sortBy(_._1).map { case (k, v) => show"$k -> $v" }.mkString_("\n")}""".stripMargin
  }
}

sealed trait Term
object Term {
  case class Const(strOrReg: Either[String, Action]) extends Term
  case class Read(ranges: CharRanges, output: Boolean) extends Term
  case class Seq(idents: List[Int]) extends Term
  case class Alternative(idents: NonEmptyList[Int]) extends Term

  def epsilon: Term = Seq(Nil)

  implicit val show: Show[Term] = Show.show {
    case Const(Left(s)) =>
      s""""$s""""
    case Const(Right(a)) =>
      a.show
    case Read(rs, true) =>
      rs.show
    case Read(rs, false) =>
      show"~$rs"
    case Seq(ids) =>
      ids.mkString_(" -> ")
    case Alternative(alts) =>
      alts.mkString_(" | ")
  }
}
