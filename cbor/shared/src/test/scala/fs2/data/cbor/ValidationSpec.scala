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
package cbor

import low._

import fs2._

import weaver._

import cats.effect._

import scodec.bits._

object ValidationSpec extends SimpleIOSuite {

  test("should fail if not enough array elements are provided") {
    Stream(CborItem.StartArray(4), CborItem.True, CborItem.Null)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail if not enough Map elements are provided") {
    Stream(CborItem.StartMap(2), CborItem.True, CborItem.Null)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail if an odd number of Map elements are provided") {
    Stream(CborItem.StartMap(2), CborItem.True, CborItem.Null, CborItem.False)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail if an odd number of indefinite map elements are provided") {
    Stream(CborItem.StartMap(2), CborItem.True, CborItem.Null, CborItem.Break)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail if unmatched break is given") {
    Stream(CborItem.True, CborItem.Break)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for unclosed indefinite array") {
    Stream(CborItem.StartIndefiniteArray, CborItem.True)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for unclosed indefinite map") {
    Stream(CborItem.StartIndefiniteMap, CborItem.True, CborItem.Null)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for unclosed indefinite text string") {
    Stream(CborItem.StartIndefiniteTextString)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for nested indefinite text string") {
    Stream(CborItem.StartIndefiniteTextString,
           CborItem.TextString(""),
           CborItem.StartIndefiniteTextString,
           CborItem.Break,
           CborItem.Break)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for invalid indefinite text string elements") {
    Stream(CborItem.StartIndefiniteTextString, CborItem.TextString(""), CborItem.True, CborItem.Break)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for unclosed indefinite byte string") {
    Stream(CborItem.StartIndefiniteByteString, CborItem.ByteString(hex""), CborItem.ByteString(hex"12"))
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for nested indefinite byte string") {
    Stream(CborItem.StartIndefiniteByteString,
           CborItem.ByteString(hex""),
           CborItem.StartIndefiniteByteString,
           CborItem.Break,
           CborItem.Break)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for invalid indefinite byte string elements") {
    Stream(CborItem.StartIndefiniteByteString, CborItem.ByteString(hex""), CborItem.True, CborItem.Break)
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for invalid number of positive integer bits") {
    Stream(CborItem.PositiveInt(hex"010101"))
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for invalid number of negative integer bits") {
    Stream(CborItem.NegativeInt(hex"010101"))
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for invalid number of half float bits") {
    Stream(CborItem.Float16(hex"010132"))
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for invalid number of float bits") {
    Stream(CborItem.Float32(hex"010132"))
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

  test("should fail for invalid number of double bits") {
    Stream(CborItem.Float64(hex"010132"))
      .through(validate[IO])
      .compile
      .drain
      .attempt
      .map(res => expect(res.isLeft))
  }

}
