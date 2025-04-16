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

import cats.effect.IO
import scodec.bits._
import weaver._
import fs2.data.msgpack.high._
import fs2.data.msgpack.high.static._
import fs2.data.msgpack.low.MsgpackItem
import java.time.Instant
import fs2.data.msgpack.high.dynamic.valuesFromItems

object DecoderSpec extends SimpleIOSuite {
  def check[A: MsgpackDecoder](inputs: ByteVector, expected: List[A]): IO[Expectations] =
    Stream
      .chunk(Chunk.byteVector(inputs))
      .through(decode[IO, A])
      .compile
      .toList
      .map(got => expect(got == expected))

  test("should work for empty compounds") {
    val input = hex"""
      80
      DE 00 00
      DF 00 00 00 00

      A0
      D9 00
      DA 00 00  
      DB 00 00 00 00

      90
      DC 00 00
      DD 00 00 00 00
      """

    val expected = (
      Map[Long, Long](),
      Map[Long, Long](),
      Map[Long, Long](),
      "",
      "",
      "",
      "",
      List[Long](),
      List[Long](),
      List[Long]()
    )

    val d = for {
      m1 <- decoder[Map[Long, Long]]
      m2 <- decoder[Map[Long, Long]]
      m3 <- decoder[Map[Long, Long]]
      s1 <- decoder[String]
      s2 <- decoder[String]
      s3 <- decoder[String]
      s4 <- decoder[String]
      l1 <- decoder[List[Long]]
      l2 <- decoder[List[Long]]
      l3 <- decoder[List[Long]]
    } yield (m1, m2, m3, s1, s2, s3, s4, l1, l2, l3)

    check(input, List(expected))(d)
  }

  test("option type") {
    val input = hex"""
      05 C0
    """

    val expected = List(Some(5), None)

    check(input, expected)
  }

  test("either type") {
    val input = hex"""
      05 a3 61 62 63
    """

    val expected = List(Left(5L), Right("abc"))

    check(input, expected)
  }

  test("extension type") {
    implicit val d: MsgpackDecoder[BitVector] = extensionDecoder { (tpe, bytes) =>
      if (tpe == 0x01)
        Some(bytes.bits)
      else
        None
    }

    val expected = List(hex"fafa".bits)

    Stream(MsgpackItem.Extension(0x01, hex"fafa"))
      .through(fromItems[IO, BitVector](d))
      .compile
      .toList
      .map(got => expect(got == expected))
  }

  test("MsgpackValue") {
    val cases = List(
      Chunk(MsgpackItem.SignedInt(hex"05")) -> MsgpackValue.Integer(5),
      Chunk(MsgpackItem.Str(hex"66 6f 6f")) -> MsgpackValue.String("foo"),
      Chunk(MsgpackItem.Array(2), MsgpackItem.True, MsgpackItem.False) ->
        MsgpackValue.Array(List(MsgpackValue.Boolean(true), MsgpackValue.Boolean(false))),
      Chunk(MsgpackItem.Bin(hex"fafb")) -> MsgpackValue.Bin(hex"fafb"),
      Chunk(MsgpackItem.Map(2),
            MsgpackItem.SignedInt(hex"1"),
            MsgpackItem.Str(hex"66 6f 6f"),
            MsgpackItem.SignedInt(hex"2"),
            MsgpackItem.Str(hex"62 61 72")) ->
        MsgpackValue.Map(
          Map(MsgpackValue.Integer(1) -> MsgpackValue.String("foo"),
              MsgpackValue.Integer(2) -> MsgpackValue.String("bar"))),
      Chunk(MsgpackItem.Float32(3.141592F)) -> MsgpackValue.Float(3.141592F),
      Chunk(MsgpackItem.Float64(3.141592)) -> MsgpackValue.Double(3.141592),
      Chunk(MsgpackItem.Timestamp96(150000000, 1741519200)) ->
        MsgpackValue.Timestamp(Instant.parse("2025-03-09T11:20:00.15Z")),
      Chunk(MsgpackItem.Extension(0x01, hex"abcdef")) -> MsgpackValue.Extension(0x01, hex"abcdef"),
      Chunk(MsgpackItem.Nil) -> MsgpackValue.Nil
    )

    Stream
      .emits(cases)
      .map { case (chunk, expected) =>
        Stream
          .chunk(chunk)
          .through(valuesFromItems[IO])
          .compile
          .lastOrError
          .map(got => expect(got == expected))
      }
      .compile
      .foldMonoid
  }
}
