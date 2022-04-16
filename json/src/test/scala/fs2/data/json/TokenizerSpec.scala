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

package fs2
package data
package json

import ast._

import weaver._

abstract class TokenizerSpec[Json](implicit builder: Builder[Json], tokenizer: Tokenizer[Json]) extends SimpleIOSuite {

  pureTest("`tokenize` and `values` should work well together") {

    val input = Stream.emit("""true {"field1": "test", "field2": 23}""")

    val toks = input.through(tokens[Fallible, String])

    val roundtrip = toks.through(values).through(tokenize)

    expect(toks.compile.toList == roundtrip.compile.toList)

  }

}
