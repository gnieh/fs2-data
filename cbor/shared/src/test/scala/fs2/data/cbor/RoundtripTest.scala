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

package fs2
package data
package cbor

import cats.Show
import fs2.data.cbor.high.CborValue
import org.scalacheck.Arbitrary
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object RoundtripTest extends SimpleIOSuite with Checkers {

  implicit val show: Show[CborValue] = Show.show(_.toString)

  test("roundtrips numbers") {
    forall(Arbitrary.arbitrary[BigInt].map(CborValue.Integer(_): CborValue)) { value =>
      Stream(value)
        .through(high.toBinary)
        .through(high.values)
        .compile
        .onlyOrError
        .map(r => expect.same(value, r))
    }
  }

}
