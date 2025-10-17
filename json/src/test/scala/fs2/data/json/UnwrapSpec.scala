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
package data
package json

import weaver.SimpleIOSuite

object UnwrapSpec extends SimpleIOSuite {

  val obj: Chunk[Token] =
    Chunk(Token.StartObject, Token.EndObject)

  pureTest("Unwrapping single chunk") {
    val unwrapped = Stream.chunk(obj).covary[Fallible]
    val wrapped = Stream.chunk(Chunk(Token.StartArray) ++ obj ++ Chunk(Token.EndArray))

    val obtained = wrapped.through(unwrap.stripTopLevelArray[fs2.Fallible]).compile.toList
    val expected = unwrapped.compile.toList

    expect.same(obtained, expected)
  }

  pureTest("Unwrapping single chunk - missing array start") {
    val wrapped = Stream.chunk(obj ++ Chunk(Token.EndArray))

    val obtained = wrapped.through(unwrap.stripTopLevelArray[fs2.Fallible]).compile.toList
    val expected = Left(JsonException("Expected start of array, got: Some(StartObject)"))

    expect.same(obtained, expected)
  }

  pureTest("Unwrapping single chunk - missing array end") {
    val wrapped = Stream.chunk(Chunk(Token.StartArray) ++ obj)

    val obtained = wrapped.through(unwrap.stripTopLevelArray[fs2.Fallible]).compile.toList
    val expected = Left(JsonException("Expected end of array, got: Some(EndObject)"))

    expect.same(obtained, expected)
  }

  pureTest("Unwrapping single chunk - missing both ends so fails on start") {
    val wrapped = Stream.chunk(obj)

    val obtained = wrapped.through(unwrap.stripTopLevelArray[fs2.Fallible]).compile.toList
    val expected = Left(JsonException("Expected start of array, got: Some(StartObject)"))

    expect.same(obtained, expected)
  }

  pureTest("Unwrapping multiple chunks") {
    val elem = Stream.chunk(obj).covary[Fallible]
    val header = Stream.chunk(Chunk(Token.StartArray) ++ obj)
    val trailer = Stream.chunk(obj ++ Chunk(Token.EndArray))

    val unwrapped = elem.repeatN(3)

    val wrapped = header ++ elem ++ trailer

    val obtained = wrapped.through(unwrap.stripTopLevelArray[fs2.Fallible]).compile.toList
    val expected = unwrapped.compile.toList

    expect.same(obtained, expected)
  }

  pureTest("Unwrapping multiple chunks - missing array start") {
    val elem = Stream.chunk(obj).covary[Fallible]
    val trailer = Stream.chunk(obj ++ Chunk(Token.EndArray))

    val wrapped = elem ++ trailer

    val obtained = wrapped.through(unwrap.stripTopLevelArray[fs2.Fallible]).compile.toList
    val expected = Left(JsonException("Expected start of array, got: Some(StartObject)"))

    expect.same(obtained, expected)
  }

  pureTest("Unwrapping multiple chunks - missing array end") {
    val elem = Stream.chunk(obj).covary[Fallible]
    val header = Stream.chunk(Chunk(Token.StartArray) ++ obj)

    val wrapped = header ++ elem

    val obtained = wrapped.through(unwrap.stripTopLevelArray[fs2.Fallible]).compile.toList
    val expected = Left(JsonException("Expected end of array, got: Some(EndObject)"))

    expect.same(obtained, expected)
  }

}
