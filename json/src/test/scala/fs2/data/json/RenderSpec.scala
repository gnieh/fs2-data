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

package fs2.data.json

import fs2._
import weaver._

object RenderSpec extends SimpleIOSuite {

  pureTest("parsing and rendering should work well together in compact form") {

    val input = Stream.emits("""true {"field1": "test", "field2": [23, [true, null]]}""")

    val toks = input.through(tokens[Fallible, Char])

    val roundtrip = toks.through(render.compact).through(tokens)

    expect(toks.compile.toList == roundtrip.compile.toList)

  }

  pureTest("parsing and rendering should work well together in pretty form") {

    val input = Stream.emits("""true {"field1": "test", "field2": [23, [true, null]]}""")

    val toks = input.through(tokens[Fallible, Char])

    val roundtrip = toks.through(render.prettyPrint()).through(tokens)

    expect(toks.compile.toList == roundtrip.compile.toList)

  }

  pureTest("a Renderer should properly escape what needs to be escaped") {
    val input = Stream.emit(Token.StringValue("some\ncharacters must\\be\"escaped\" like ß")).covaryOutput[Token]

    expect.same("\"some\\ncharacters must\\\\be\\\"escaped\\\" like \\u00df\"",
                input.through(render.prettyPrint()).compile.string)

  }

  pureTest("An object should be properly pretty renderer with line width of 10") {
    val input = Stream.emits("""{"field1": "test", "field2": [23, [true, null]]}""")

    expect.same(
      Right("""{
              |  "field1": "test",
              |  "field2": [
              |    23,
              |    [
              |      true,
              |      null
              |    ]
              |  ]
              |}""".stripMargin),
      input.through(tokens[Fallible, Char]).through(fs2.data.text.render.pretty(width = 10, indent = 2)).compile.string
    )

  }

  pureTest("An object should be properly pretty renderer with line width of 32") {
    val input = Stream.emits("""{"field1": "test", "field2": [23, [true, null]]}""")

    expect.same(
      Right("""{
              |  "field1": "test",
              |  "field2": [23, [true, null]]
              |}""".stripMargin),
      input.through(tokens[Fallible, Char]).through(fs2.data.text.render.pretty(width = 32, indent = 2)).compile.string
    )

  }

  pureTest("An object should be properly pretty renderer with line width of 80") {
    val input = Stream.emits("""{
                               |  "field1": "test",
                               |  "field2": [23, [true, null]]}""".stripMargin)

    expect.same(
      Right("""{"field1": "test", "field2": [23, [true, null]]}"""),
      input.through(tokens[Fallible, Char]).through(fs2.data.text.render.pretty(width = 80, indent = 2)).compile.string
    )
  }

  pureTest("An object should be properly pretty renderer with line width of 80") {
    val input = Stream.emits("""{
                               |  "field1": "test",
                               |  "field2": [23, [true, null]]}""".stripMargin)

    expect.same(
      Right("""{"field1": "test", "field2": [23, [true, null]]}"""),
      input.through(tokens[Fallible, Char]).through(fs2.data.text.render.pretty(width = 80, indent = 2)).compile.string
    )

  }

}
