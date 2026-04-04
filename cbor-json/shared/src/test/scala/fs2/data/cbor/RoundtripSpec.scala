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

import cats.effect.IO
import fs2.data.cbor.high.CborValue
import weaver.SimpleIOSuite

object RoundtripSpec extends SimpleIOSuite {

  test("CBOR items should round-trip through JSON") {
    val items = List(
      CborValue.Integer(123456), // 3 bytes
      CborValue.Integer(1235213352876L), // 5 bytes
      CborValue.TextString("Hello, world!"),
      CborValue.True,
      CborValue.False,
      CborValue.Null,
      CborValue.Float64(Math.PI)
    )

    Stream
      .emits(items)
      .through(cbor.high.toItems)
      .through(cbor.json.decodeItems[IO])
      .through(json.cbor.encodeItems)
      .through(cbor.high.parseValues)
      .compile
      .toList
      .map { result => expect.same(items, result) }
  }

}
