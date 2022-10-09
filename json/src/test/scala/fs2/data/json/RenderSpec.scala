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

package fs2.data.json

import internals._

import fs2._

import weaver._

object RenderSpec extends SimpleIOSuite {

  pureTest("parsing and rendering should work well together in compact form") {

    val input = Stream.emits("""true {"field1": "test", "field2": [23, [true, null]]}""")

    val toks = input.through(tokens[Fallible, Char])

    val roundtrip = toks.through(render.compact).flatMap(Stream.emits(_)).through(tokens)

    expect(toks.compile.toList == roundtrip.compile.toList)

  }

  pureTest("parsing and rendering should work well together in pretty form") {

    val input = Stream.emits("""true {"field1": "test", "field2": [23, [true, null]]}""")

    val toks = input.through(tokens[Fallible, Char])

    val roundtrip = toks.through(render.pretty()).flatMap(Stream.emits(_)).through(tokens)

    expect(toks.compile.toList == roundtrip.compile.toList)

  }

  pureTest("a Renderer should properly escape what needs to be escaped") {
    val renderer = new Renderer(true, true, "")

    renderer += Chunk.singleton(Token.StringValue("some\ncharacters must\\be\"escaped\" like ß"))

    val res = renderer.result

    expect(res == "\"some\\ncharacters must\\\\be\\\"escaped\\\" like \\u00df\"")

  }

}
