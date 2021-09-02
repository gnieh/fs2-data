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

package fs2
package data
package text

import weaver._

object CharRangesSuite extends SimpleIOSuite {

  pureTest("Disjoint ranges should be added disjointly") {
    val r1 = CharRange.unsafeOf('a' -> 'd')
    val r2 = CharRange.unsafeOf('f' -> 'h')
    expect.eql(expected = List(r1, r2), found = CharRanges.of(r2, r1).ranges)
  }

  pureTest("Overlapping ranges should be merged") {
    val r1 = CharRange.unsafeOf('a' -> 'd')
    val r2 = CharRange.unsafeOf('c' -> 'h')
    expect.eql(expected = List(CharRange.unsafeOf('a' -> 'h')), found = CharRanges.of(r2, r1).ranges)
  }

  pureTest("Touching ranges should be merged") {
    val r1 = CharRange.unsafeOf('a' -> 'd')
    val r2 = CharRange.unsafeOf('d' -> 'h')
    expect.eql(expected = List(CharRange.unsafeOf('a' -> 'h')), found = CharRanges.of(r1, r2).ranges)
  }

}
