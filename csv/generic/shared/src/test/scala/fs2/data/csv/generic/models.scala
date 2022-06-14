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

package fs2.data.csv
package generic

sealed trait Simple
case object On extends Simple
case object Off extends Simple

sealed trait Complex
case object Active extends Complex
//object Inactive extends Complex
case class Numbered(n: Int) extends Complex
case class Unknown(state: String) extends Complex

sealed trait Alphabet
@CsvValue("A") case object Alpha extends Alphabet
@CsvValue("B") case object Beta extends Alphabet
case object Gamma extends Alphabet

case class IntWrapper(value: Int)
case class IntResultWrapper(value: DecoderResult[Int])
case class Thing(value: String, extra: Int)
case class ThingWrapper(thing: Thing)
case class Wrapper[T](value: T)
