/*
 * Copyright 2019-2022 Lucas Satabin
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

import weaver._

object LiteralEncodersTest extends SimpleIOSuite {

  // The literal encoders should be present implicitly
  CellEncoder["foo"]
  CellEncoder[1]
  CellEncoder[true]
  CellEncoder['C']

  pureTest("The literal encoders should encode the precise same value") {
    expect(CellEncoder["foo"].apply("foo") == "foo") and
      expect(CellEncoder[1].apply(1) == "1") and
      expect(CellEncoder[true].apply(true) == "true") and
      expect(CellEncoder['C'].apply('C') == "C")
  }

}
