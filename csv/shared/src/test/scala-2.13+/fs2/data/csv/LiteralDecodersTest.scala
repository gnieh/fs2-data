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

package fs2.data.csv

import weaver._

object LiteralDecodersTest extends SimpleIOSuite {

  // The literal decoders should be present implicitly
  CellDecoder["foo"]
  CellDecoder[1]
  CellDecoder[true]
  CellDecoder['C']

  pureTest("The literal decoders should parse the precise same value") {
    expect(CellDecoder["foo"].apply("foo") == Right("foo")) and
      expect(CellDecoder[1].apply("1") == Right(1)) and
      expect(CellDecoder[true].apply("true") == Right(true)) and
      expect(CellDecoder['C'].apply("C") == Right('C'))
  }

  pureTest("The literal decoders should fail for other values of the same primitive type") {
    expect(CellDecoder["foo"].apply("bar").isLeft == true) and
      expect(CellDecoder[1].apply("2").isLeft == true) and
      expect(CellDecoder[true].apply("false").isLeft == true) and
      expect(CellDecoder['C'].apply("D").isLeft == true)
  }

}
