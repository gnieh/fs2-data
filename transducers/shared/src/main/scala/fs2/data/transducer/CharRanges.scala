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

object CharRanges {

  /** The empty set of character ranges */
  val empty: CharRanges = RangeSet.empty

  /** The set that contains all characters */
  val all: CharRanges = RangeSet.all

  /** Creates a singleton set of a singleton range */
  def char(c: Char): CharRanges =
    RangeSet.char(c)

  /** Creates a set of ranges based on the provided single characters */
  def chars(c1: Char, c2: Char, cs: Char*): CharRanges =
    RangeSet.chars(c1, c2, cs: _*)

  /** Creates a singleton set of ranges */
  def range(r: (Char, Char)): CharRanges =
    RangeSet.range(r)

  /** Creates a set of ranges */
  def ranges(r1: (Char, Char), r2: (Char, Char), rs: (Char, Char)*): CharRanges =
    RangeSet.ranges(r1, r2, rs: _*)

}
