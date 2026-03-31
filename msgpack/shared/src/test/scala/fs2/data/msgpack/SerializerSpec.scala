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
package data.msgpack

import low.MsgpackItem
import high._

import cats.effect.IO
import scodec.bits._
import weaver._

object SeserializerSpec extends SimpleIOSuite {
  def checkPairs[A: MsgpackSerializer](cases: (A, List[MsgpackItem])*): IO[Expectations] =
    Stream
      .emits(cases)
      .evalMap { case (input, expected) =>
        Stream
          .emit(input)
          .through(fs2.data.msgpack.high.toItems[IO, A])
          .compile
          .toList
          .map(expect.same(expected, _))
      }
      .compile
      .foldMonoid

  test("should correctly serialize integer types") {
    for {
      bytes <- checkPairs(
        Byte.MinValue -> List(MsgpackItem.SignedInt(hex"80")),
        Byte.MaxValue -> List(MsgpackItem.SignedInt(hex"7f"))
      )

      shorts <- checkPairs(
        Short.MaxValue -> List(MsgpackItem.SignedInt(hex"7fff")),
        Short.MinValue -> List(MsgpackItem.SignedInt(hex"8000")),
        Byte.MaxValue.toShort -> List(MsgpackItem.SignedInt(hex"7f")),
        (Byte.MaxValue.toShort + 1).toShort -> List(MsgpackItem.UnsignedInt(hex"80")),
        (Byte.MinValue.toShort - 1).toShort -> List(MsgpackItem.SignedInt(hex"ff7f"))
      )

      ints <- checkPairs(
        0x7f -> List(MsgpackItem.SignedInt(hex"7f")),
        0x80 -> List(MsgpackItem.UnsignedInt(hex"80")), // positive fixint bondry
        0x81 -> List(MsgpackItem.UnsignedInt(hex"81")),
        -2 -> List(MsgpackItem.SignedInt(hex"fe")),
        -32 -> List(MsgpackItem.SignedInt(hex"e0")), // negative fixint boundry
        -33 -> List(MsgpackItem.SignedInt(hex"df")),
        -128 -> List(MsgpackItem.SignedInt(hex"80")),
        Int.MaxValue -> List(MsgpackItem.SignedInt(hex"7fffffff")),
        Int.MinValue -> List(MsgpackItem.SignedInt(hex"80000000"))
      )

      longs <- checkPairs(
        0x01L -> List(MsgpackItem.SignedInt(hex"01")),
        0x81L -> List(MsgpackItem.UnsignedInt(hex"81")),
        Int.MaxValue.toLong + 1 -> List(MsgpackItem.UnsignedInt(hex"80000000")),
        Int.MinValue.toLong - 1 -> List(MsgpackItem.SignedInt(hex"ffffffff 7fffffff")),
        Long.MinValue -> List(MsgpackItem.SignedInt(hex"80000000 00000000")),
        Long.MaxValue -> List(MsgpackItem.SignedInt(hex"7fffffff ffffffff"))
      )

      bigints <- checkPairs(
        BigInt(1) -> List(MsgpackItem.SignedInt(hex"01")),
        BigInt(Long.MinValue) -> List(MsgpackItem.SignedInt(hex"80000000 00000000")),
        BigInt(Long.MaxValue) -> List(MsgpackItem.SignedInt(hex"7fffffff ffffffff")),
        BigInt(Long.MaxValue) + 1 -> List(MsgpackItem.UnsignedInt(hex"80000000 00000000"))
      )
    } yield bytes && shorts && ints && longs && bigints
  }

  test("should correctly serialize MsgpackValues") {
    import ast.MsgpackValue

    val cases = Seq(
      MsgpackValue.Integer(5) -> List(MsgpackItem.SignedInt(hex"05")),
      MsgpackValue.String("foo") -> List(MsgpackItem.Str(hex"666f6f")),

      // simple list
      MsgpackValue.Array(
        List(
          MsgpackValue.Nil,
          MsgpackValue.Integer(5)
        )) -> List(MsgpackItem.Array(2), MsgpackItem.Nil, MsgpackItem.SignedInt(hex"05")),

      // nested structures
      (MsgpackValue.Map(
         Map(
           MsgpackValue.String("active") -> MsgpackValue.Boolean(true),
           MsgpackValue.String("age") -> MsgpackValue.Integer(16),
           MsgpackValue.Extension(15, hex"ae") -> MsgpackValue.Array(
             List(
               MsgpackValue.Boolean(true),
               MsgpackValue.Double(3.14),
               MsgpackValue.Boolean(false)
             ))
         )),
       List(
         MsgpackItem.Map(3),
         MsgpackItem.Str(hex"616374697665"),
         MsgpackItem.True,
         MsgpackItem.Str(hex"616765"),
         MsgpackItem.SignedInt(hex"10"),
         MsgpackItem.Extension(15, hex"ae"),
         MsgpackItem.Array(3),
         MsgpackItem.True,
         MsgpackItem.Float64(3.14),
         MsgpackItem.False
       ))
    )

    Stream
      .emits(cases)
      .evalMap { case (input, expected) =>
        Stream
          .emit(input)
          .through(fs2.data.msgpack.high.ast.valuesToItems)
          .compile
          .toList
          .map(expect.same(expected, _))
      }
      .compile
      .foldMonoid
  }
}
