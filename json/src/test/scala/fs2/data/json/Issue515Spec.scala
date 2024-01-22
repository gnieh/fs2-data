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
import fs2.data.text.utf8._
import weaver.SimpleIOSuite

import java.nio.charset.StandardCharsets

object Issue515Spec extends SimpleIOSuite {

  // Tests the fix for https://github.com/gnieh/fs2-data/issues/515
  // Problem was an escape sequence on a chunk boundary leading to outputting parts of the output buffer twice.
  test("not duplicate tokens for escaped chars on chunk boundaries") {
    val chunkSize = 55
    val input =
      """{"FilePaths":["\\\\path\\to\\some\\file.txt","\\\\path\\to\\some\\other\\file.txt","\\\\path\\to\\another\\file.txt"]}"""
    val stream: fs2.Stream[IO, Byte] = Stream.emits(input.getBytes(StandardCharsets.UTF_8))
    stream
      .chunkLimit(chunkSize)
      .flatMap(Stream.chunk)
      .through(tokens[IO, Byte])
      .through(render.compact)
      .compile
      .foldMonoid
      .map(out => expect.eql(input, out))
  }

}
