/*
 * Copyright 2023 Lucas Satabin
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

package fs2.data.cbor.high

import cats.effect.IO
import fs2._
import weaver.SimpleIOSuite

object ValueParserTest extends SimpleIOSuite {

  test("roundtrips bignums") {
    val in = CborValue.Integer(BigInt("-739421513997118914047232662242593364"))
    Stream(in).covary[IO].through(toBinary).through(values).compile.onlyOrError.map { out =>
      expect.same(in, out)
    }
  }

}
