/*
 * Copyright 2024 fs2-data Project
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
package data.json

import cats.effect.IO
import weaver.SimpleIOSuite

object Issue745Spec extends SimpleIOSuite {

  // Tests the fix for https://github.com/gnieh/fs2-data/issues/745
  // Same root cause as #515, but for the String chunk path.
  // The fast-path string_ method was missing T.mark(context) after seeing a backslash,
  // so when a chunk boundary fell right after the backslash, appendMarked in slowString_
  // would re-append already-consumed content, duplicating part of the string.

  test("not duplicate tokens for escaped quotes on String chunk boundaries") {
    val input = """{"message":"said \"hello\""}"""
    // Try every possible 2-chunk split position
    (1 until input.length).toList.foldLeft(IO.pure(expect(true))) { (acc, splitAt) =>
      acc.flatMap { prev =>
        val (a, b) = input.splitAt(splitAt)
        (Stream.emit(a) ++ Stream.emit(b))
          .through(tokens[IO, String])
          .through(render.compact)
          .compile
          .foldMonoid
          .map(out => prev and expect.eql(input, out))
      }
    }
  }

  test("not duplicate tokens for backslash-backslash on String chunk boundaries") {
    val input =
      """{"FilePaths":["\\\\path\\to\\some\\file.txt","\\\\path\\to\\some\\other\\file.txt","\\\\path\\to\\another\\file.txt"]}"""
    (1 until input.length).toList.foldLeft(IO.pure(expect(true))) { (acc, splitAt) =>
      acc.flatMap { prev =>
        val (a, b) = input.splitAt(splitAt)
        (Stream.emit(a) ++ Stream.emit(b))
          .through(tokens[IO, String])
          .through(render.compact)
          .compile
          .foldMonoid
          .map(out => prev and expect.eql(input, out))
      }
    }
  }

}
