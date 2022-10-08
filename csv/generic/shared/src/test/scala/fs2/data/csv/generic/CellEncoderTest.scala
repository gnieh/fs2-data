/*
 * Copyright 2022 Lucas Satabin
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

import fs2.data.csv.CellEncoder
import weaver._

import scala.annotation.nowarn

case class WithDef(a: Int = 1, b: String = "BBBB")

@nowarn object CellEncoderTest extends SimpleIOSuite {

  pureTest("derivation for coproducts should work out of the box for enum-style sealed traits") {
    val simpleEncoder: CellEncoder[Simple] = semiauto.deriveCellEncoder
    expect(simpleEncoder(On) == "On") and
      expect(simpleEncoder(Off) == "Off")
  }

  pureTest("derivation for coproducts should handle non-case object cases") {
    implicit val numberedEncoder: CellEncoder[Numbered] =
      CellEncoder[Int].contramap(_.n)
    implicit val unknownEncoder: CellEncoder[Unknown] =
      CellEncoder[String].contramap(_.state)
    val complexEncoder: CellEncoder[Complex] = semiauto.deriveCellEncoder

    expect(complexEncoder(Active) == "Active") and
      // expect(complexEncoder(Inactive) == "Inactive") and
      expect(complexEncoder(Unknown("inactive")) == "inactive") and
      expect(complexEncoder(Numbered(7)) == "7") and
      expect(complexEncoder(Unknown("foo")) == "foo")
  }

  pureTest("derivation for coproducts should respect @CsvValue annotations") {
    val alphabetEncoder: CellEncoder[Alphabet] = semiauto.deriveCellEncoder

    expect(alphabetEncoder(Alpha) == "A") and
      expect(alphabetEncoder(Beta) == "B") and
      expect(alphabetEncoder(Gamma) == "Gamma")
  }

  // TODO: Do we want that? Scala 2 doesn't do it like that (yet)
  /*pureTest("derivation for coproducts should respect existing encoders") {
    implicit val alphaEncoder: CellEncoder[Alpha.type] = CellEncoder.const("CUSTOM")
    val alphabetEncoder: CellEncoder[Alphabet] = semiauto.deriveCellEncoder

    expect(alphabetEncoder(Alpha) == "CUSTOM") and
      expect(alphabetEncoder(Beta) == "B") and
      expect(alphabetEncoder(Gamma) == "Gamma")
  }*/

  pureTest("derivation for unary products should work for standard types") {
    val result = semiauto.deriveCellEncoder[IntWrapper].apply(IntWrapper(7))
    expect(result == "7")
  }

  pureTest("derivation for unary products should work for types with implicit encoder") {
    implicit val thingEncoder: CellEncoder[Thing] =
      CellEncoder[String].contramap(_.value)
    val result = semiauto
      .deriveCellEncoder[ThingWrapper]
      .apply(ThingWrapper(Thing("cell", 7)))
    expect(result == "cell")
  }

  pureTest("derivation for unary products should work for types with arguments") {
    val result = semiauto.deriveCellEncoder[Wrapper[Int]].apply(Wrapper(7))
    expect(result == "7")
  }
}
