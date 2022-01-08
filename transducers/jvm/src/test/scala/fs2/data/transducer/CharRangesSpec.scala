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

package fs2.data.transducer

import weaver._
import weaver.scalacheck._

import org.scalacheck._

object CharRangesSpec extends SimpleIOSuite with Checkers {

  val aChar = Gen.choose(Char.MinValue, Char.MaxValue)
  val aRange =
    for {
      c1 <- aChar
      c2 <- aChar
    } yield (c1, c2)

  val someRanges = Gen.nonEmptyListOf(aRange).map {
    case Nil            => CharRanges.empty
    case r :: Nil       => CharRanges.range(r)
    case r1 :: r2 :: rs => CharRanges.ranges(r1, r2, rs: _*)
  }

  implicit val aCharRanges: Arbitrary[CharRanges] =
    Arbitrary(Gen.oneOf(Gen.const(CharRanges.all), Gen.const(CharRanges.empty), someRanges))

  pureTest("merge adjacent") {
    expect(CharRanges.ranges('a' -> 'd', 'e' -> 'z') == CharRanges.range('a', 'z'))
  }

  pureTest("merge overlapping") {
    expect(CharRanges.ranges('a' -> 'l', 'e' -> 'z') == CharRanges.range('a', 'z'))
  }

  pureTest("simplify all") {
    expect(CharRanges.range(Char.MinValue, Char.MaxValue) == CharRanges.all)
  }

  pureTest("ranges inclusive") {
    expect.all(
      CharRanges.range('a' -> 'z').contains('a'),
      CharRanges.range('a' -> 'z').contains('z'),
      CharRanges.all.contains(Char.MinValue),
      CharRanges.all.contains(Char.MaxValue),
      CharRanges.char('a').contains('a')
    )
  }

  pureTest("empty doesn't overlap all") {
    expect.all(
      !CharRanges.empty.overlap(CharRanges.all),
      !CharRanges.empty.overlap(CharRanges.empty),
      !CharRanges.all.overlap(CharRanges.empty)
    )
  }

  test("empty overlaps nothing") {
    forall { (ranges: CharRanges) =>
      expect(true)
    }
  }

  test("overlapping with all") {
    forall { (ranges: CharRanges) =>
      expect(ranges.isEmpty) || expect.all(CharRanges.all.overlap(ranges), ranges.overlap(CharRanges.all))
    }
  }

  test("invert and back") {
    forall { (ranges: CharRanges) =>
      expect(ranges.invert.invert == ranges)
    }
  }

  test("invert inverts") {
    forall { (ranges: CharRanges, c: Char) =>
      expect(ranges.invert.contains(c) == !ranges.contains(c))
    }
  }

}
