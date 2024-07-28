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

package fs2.data.csv

import cats.data.NonEmptyList
import weaver.*

object RowGeneratorTest extends SimpleIOSuite {

  pureTest("Emit error on wrong row size (#621)") {
    val input = List(
      Row(NonEmptyList.of("a", "b", "c"), Some(1)),
      Row(NonEmptyList.of("d", "e"), Some(2)),
      Row(NonEmptyList.of("f", "g", "h", "i"), Some(3)),
      Row(NonEmptyList.of("j", "k", "l"), Some(4))
    )
    val headers = NonEmptyList.of("first", "second", "third")

    val result = fs2.Stream.emits(input).through(lowlevel.attemptWriteWithGivenHeaders(headers)).compile.toList

    matches(result) {
      case List(
            Right(NonEmptyList("first", "second" :: "third" :: Nil)),
            Right(NonEmptyList("a", "b" :: "c" :: Nil)),
            Left(e1: HeaderSizeError),
            Left(e2: HeaderSizeError),
            Right(NonEmptyList("j", "k" :: "l" :: Nil))
          ) =>
        expect.all(e1.expectedColumns == 3,
                   e1.actualColumns == 2,
                   e1.line == Some(2L),
                   e2.expectedColumns == 3,
                   e2.actualColumns == 4,
                   e2.line == Some(3L))
    }
  }
}
