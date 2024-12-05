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
          .redeem(expect.same(_, rhs), _ => failure(s"Expected error for item ${lhs}"))
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
          .redeem(expect.same(_, rhs), _ => failure(s"Expected error for item ${lhs}"))
      }
      .compile
      .foldMonoid

  test("should raise if integer values exceed 64 bits") {
    validation1(
      MsgpackItem.UnsignedInt(hex"10000000000000000") ->
        MsgpackMalformedItemException("Unsigned int exceeds 64 bits", Some(0)),
      MsgpackItem.SignedInt(hex"10000000000000000") ->
        MsgpackMalformedItemException("Signed int exceeds 64 bits", Some(0))
    )
  }

  test("should raise if string or binary values exceed 2^32 - 1 bytes") {
    validation1(
      MsgpackItem.Str(ByteVector.empty.padLeft(Math.pow(2, 32).toLong)) ->
        MsgpackMalformedItemException("String exceeds (2^32)-1 bytes", Some(0)),
      MsgpackItem.Bin(ByteVector.empty.padLeft(Math.pow(2, 32).toLong)) ->
        MsgpackMalformedItemException("Bin exceeds (2^32)-1 bytes", Some(0))
    )
  }

  test("should raise on unexpected end of input") {
    validation(
      List(MsgpackItem.Array(2), MsgpackItem.True) ->
        MsgpackUnexpectedEndOfStreamException(Some(0)),
      List(MsgpackItem.Array(2), MsgpackItem.Array(1), MsgpackItem.True) ->
        MsgpackUnexpectedEndOfStreamException(Some(0)),
      List(MsgpackItem.Array(1), MsgpackItem.Array(1)) ->
        MsgpackUnexpectedEndOfStreamException(Some(1)),
      List(MsgpackItem.Array(0), MsgpackItem.Array(1)) ->
        MsgpackUnexpectedEndOfStreamException(Some(1)),
      List(MsgpackItem.Map(1), MsgpackItem.True) ->
        MsgpackUnexpectedEndOfStreamException(Some(0)),
      List(MsgpackItem.Map(1), MsgpackItem.Map(1), MsgpackItem.True, MsgpackItem.True) ->
        MsgpackUnexpectedEndOfStreamException(Some(0)),
      List(MsgpackItem.Map(2), MsgpackItem.True, MsgpackItem.Map(1)) ->
        MsgpackUnexpectedEndOfStreamException(Some(2)),
      List(MsgpackItem.Map(2), MsgpackItem.True, MsgpackItem.Map(1)) ->
        MsgpackUnexpectedEndOfStreamException(Some(2)),
      List(MsgpackItem.Map(0), MsgpackItem.Map(1)) ->
        MsgpackUnexpectedEndOfStreamException(Some(1))
    )
  }

  test("should raise if extension data exceeds 2^32 - 1 bytes") {
    validation1(
      MsgpackItem.Extension(0x54, ByteVector.empty.padLeft(Math.pow(2, 32).toLong)) ->
        MsgpackMalformedItemException("Extension data exceeds (2^32)-1 bytes", Some(0))
    )
  }

  test("should raise if nanoseconds fields exceed 999999999") {
    validation1(
      MsgpackItem.Timestamp64(0xee6b280000000000L) ->
        MsgpackMalformedItemException("Timestamp64 nanoseconds is larger than '999999999'", Some(0)),
      MsgpackItem.Timestamp96(1000000000, 0) ->
        MsgpackMalformedItemException("Timestamp96 nanoseconds is larger than '999999999'", Some(0))
    )
  }
}
