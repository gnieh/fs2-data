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

  test("not duplicate tokens for escaped quotes on String chunk boundaries") {
    val input = """{"message":"said \"hello\""}"""
    Stream
      .range(1, input.length)
      .evalMap { splitAt =>
        val (a, b) = input.splitAt(splitAt)
        (Stream.emit(a) ++ Stream.emit(b))
          .through(tokens[IO, String])
          .through(render.compact)
          .compile
          .string
          .map(out => expect.eql(input, out))
      }
      .compile
      .foldMonoid
  }

  test("not duplicate tokens for backslash-backslash on String chunk boundaries") {
    val input =
      """{"FilePaths":["\\\\path\\to\\some\\file.txt","\\\\path\\to\\some\\other\\file.txt","\\\\path\\to\\another\\file.txt"]}"""
    Stream
      .range(1, input.length)
      .evalMap { splitAt =>
        val (a, b) = input.splitAt(splitAt)
        (Stream.emit(a) ++ Stream.emit(b))
          .through(tokens[IO, String])
          .through(render.compact)
          .compile
          .string
          .map(out => expect.eql(input, out))
      }
      .compile
      .foldMonoid
  }

}
