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
package msgpack

import cats.effect._
import low.MsgpackItem
import fs2.data.msgpack.low.internal.{ValidationError, ValidationErrorAt}
import scodec.bits.ByteVector
import weaver._
import scodec.bits._

import cats.implicits._

object ValidationSpec extends SimpleIOSuite {
  def validation1[F[_]: Sync](cases: (MsgpackItem, Throwable)*): F[Expectations] =
    Stream
      .emits(cases)
      .evalMap { case (lhs, rhs) =>
        Stream
          .emit(lhs)
          .through(low.toBinary[F])
          .compile
          .drain
          .map(_ => failure(s"Expected error for item ${lhs}"))
          .handleError(expect.same(_, rhs))
      }
      .compile
      .foldMonoid

  def validation[F[_]: Sync](cases: (List[MsgpackItem], Throwable)*): F[Expectations] =
    Stream
      .emits(cases)
      .evalMap { case (lhs, rhs) =>
        Stream
          .emits(lhs)
          .through(low.toBinary[F])
          .compile
          .drain
          .map(_ => failure(s"Expected error for item ${lhs}"))
          .handleError(expect.same(_, rhs))
      }
      .compile
      .foldMonoid


  test("should raise if integer values exceed 64 bits") {
    validation1(
      MsgpackItem.UnsignedInt(hex"10000000000000000") -> new ValidationErrorAt(0, "Unsigned int exceeds 64 bits"),
      MsgpackItem.SignedInt(hex"10000000000000000")-> new ValidationErrorAt(0, "Signed int exceeds 64 bits")
    )
  }

  test("should raise if string or binary values exceed 2^32 - 1 bytes") {
    validation1(
      MsgpackItem.Str(ByteVector.empty.padLeft(Math.pow(2, 32).toLong)) -> new ValidationErrorAt(0, "String exceeds (2^32)-1 bytes"),
      MsgpackItem.Bin(ByteVector.empty.padLeft(Math.pow(2, 32).toLong)) -> new ValidationErrorAt(0, "Bin exceeds (2^32)-1 bytes"),
    )
  }

  test("should raise on unexpected end of input") {
    validation(
      List(MsgpackItem.Array(2), MsgpackItem.True) -> new ValidationError("Unexpected end of input (starting at 0)"),
      List(MsgpackItem.Array(2), MsgpackItem.Array(1), MsgpackItem.True) -> new ValidationError("Unexpected end of input (starting at 0)"),
      List(MsgpackItem.Array(1), MsgpackItem.Array(1)) -> new ValidationError("Unexpected end of input (starting at 1)"),
      List(MsgpackItem.Array(0), MsgpackItem.Array(1)) -> new ValidationError("Unexpected end of input (starting at 1)"),

      List(MsgpackItem.Map(1), MsgpackItem.True) -> new ValidationError("Unexpected end of input (starting at 0)"),
      List(MsgpackItem.Map(1), MsgpackItem.Map(1), MsgpackItem.True, MsgpackItem.True) -> new ValidationError("Unexpected end of input (starting at 0)"),
      List(MsgpackItem.Map(2), MsgpackItem.True, MsgpackItem.Map(1)) -> new ValidationError("Unexpected end of input (starting at 2)"),
      List(MsgpackItem.Map(2), MsgpackItem.True, MsgpackItem.Map(1)) -> new ValidationError("Unexpected end of input (starting at 2)"),
      List(MsgpackItem.Map(0), MsgpackItem.Map(1)) -> new ValidationError("Unexpected end of input (starting at 1)"),
    )
  }

  test("should raise if extension data exceeds 2^32 - 1 bytes") {
    validation1(
      MsgpackItem.Extension(0x54, ByteVector.empty.padLeft(Math.pow(2, 32).toLong)) -> new ValidationErrorAt(0, "Extension data exceeds (2^32)-1 bytes"),
    )
  }

  test("should raise if nanoseconds fields exceed 999999999") {
    validation1(
      MsgpackItem.Timestamp64(0xEE6B280000000000L)-> new ValidationErrorAt(0, "Timestamp64 nanoseconds cannot be larger than '999999999'"),
      MsgpackItem.Timestamp96(1000000000, 0)-> new ValidationErrorAt(0, "Timestamp96 nanoseconds cannot be larger than '999999999'"),
    )
  }
}
