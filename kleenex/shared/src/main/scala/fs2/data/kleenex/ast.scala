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

package fs2.data.kleenex

import cats.data.NonEmptyList

import cats.parse.Caret

case class Program(pipeline: NonEmptyList[String], productions: NonEmptyList[Production])(val pos: Caret)

case class Production(name: String, term: Term)(val pos: Caret)

sealed trait Term {
  val pos: Caret
}
object Term {
  case class One()(val pos: Caret) extends Term
  case class Str(s: String)(val pos: Caret) extends Term
  case class Var(name: String)(val pos: Caret) extends Term
  case class Capture(reg: String, inner: Term)(val pos: Caret) extends Term
  case class Output(reg: String)(val pos: Caret) extends Term
  case class UpdateReg(reg: String, value: NonEmptyList[RegOrStr])(val pos: Caret) extends Term
  case class Alternative(cases: NonEmptyList[Term]) extends Term {
    val pos: Caret = cases.head.pos
  }
  case class Concat(terms: NonEmptyList[Term]) extends Term {
    val pos: Caret = terms.head.pos
  }
  case class RE(re: Regex)(val pos: Caret) extends Term
  case class Suppress(inner: Term)(val pos: Caret) extends Term
  case class Star(inner: Term)(val pos: Caret) extends Term
  case class Plus(inner: Term)(val pos: Caret) extends Term
  case class Question(inner: Term)(val pos: Caret) extends Term
  case class Range(inner: Term, min: Int, max: Option[Int])(val pos: Caret) extends Term
}

sealed trait RegOrStr
object RegOrStr {
  case class Reg(name: String) extends RegOrStr
  case class Str(s: String) extends RegOrStr
}
