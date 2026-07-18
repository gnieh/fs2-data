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

// This test deliberately lives *outside* the `fs2.data.csv.generic` package to reproduce
// the call site of a downstream user. `deriveCsvRowDecoder`/`deriveCsvRowEncoder` summon
// the `Names[T]` given here; if that given is `private[generic]` it is only found through
// a deprecated fallback that Scala 3.10 removes, which would turn the lines below into
// compile errors. Keeping this test in a foreign package guards against that regression.
// See https://github.com/gnieh/fs2-data/issues/779
package fs2.data.csv.regression

import fs2.data.csv.{CsvRow, CsvRowDecoder, CsvRowEncoder}
import fs2.data.csv.generic.CsvName
import fs2.data.csv.generic.semiauto.{deriveCsvRowDecoder, deriveCsvRowEncoder}

import cats.data.NonEmptyList
import weaver._

object InaccessibleNamesRegressionTest extends SimpleIOSuite {

  case class Foo(i: Int, s: String, @CsvName("j") k: Int)

  // These derivations summon `Names[Foo]` from outside `fs2.data.csv.generic`; they must
  // resolve without relying on an inaccessible given.
  given CsvRowDecoder[Foo, String] = deriveCsvRowDecoder
  given CsvRowEncoder[Foo, String] = deriveCsvRowEncoder

  val row = CsvRow.unsafe(NonEmptyList.of("1", "test", "42"), NonEmptyList.of("i", "s", "j"))

  pureTest("a decoder derived outside the generic package decodes by (renamed) header") {
    expect(CsvRowDecoder[Foo, String].apply(row) == Right(Foo(1, "test", 42)))
  }

  pureTest("an encoder derived outside the generic package round-trips with the decoder") {
    val encoded = CsvRowEncoder[Foo, String].apply(Foo(1, "test", 42))
    expect(CsvRowDecoder[Foo, String].apply(encoded) == Right(Foo(1, "test", 42)))
  }

}
