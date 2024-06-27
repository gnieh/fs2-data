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

package fs2.data.msgpack

import cats.effect.IO
import scodec.bits.*
import weaver.SimpleIOSuite
import fs2.*
import fs2.data.msgpack.low.MsgpackItem
import java.nio.charset.StandardCharsets

object ParserSpec extends SimpleIOSuite {
  test("MessagePack value parser should correctly parse all formats") {
    val cases: List[(ByteVector, List[MsgpackItem])] = List(
      // positive fixint
      (hex"70", List(MsgpackItem.SignedInt(hex"70"))),

      // fixmap
      (hex"0x8a8d", List(MsgpackItem.Map(10), MsgpackItem.Map(13))),

      // fixarr
      (hex"0x9a9d", List(MsgpackItem.Array(10), MsgpackItem.Array(13))),
      (hex"930c0d0e",
       List(MsgpackItem.Array(3),
            MsgpackItem.SignedInt(ByteVector(12)),
            MsgpackItem.SignedInt(ByteVector(13)),
            MsgpackItem.SignedInt(ByteVector(14)))),

      // fixstr
      (hex"0xa26e6f", List(MsgpackItem.Str(hex"6e6f"))),

      // nil
      (hex"c0", List(MsgpackItem.Nil)),

      // true and false
      (hex"c3c2", List(MsgpackItem.True, MsgpackItem.False)),

      // bin8, bin16, bin32
      (hex"0xc4021234", List(MsgpackItem.Bin(hex"1234"))),
      (hex"0xc500041234abba", List(MsgpackItem.Bin(hex"1234abba"))),
      (hex"0xc6000000041234abba", List(MsgpackItem.Bin(hex"1234abba"))),

      // ext8, ext16, ext32
      (hex"0xc702121234", List(MsgpackItem.Extension(0x12, hex"1234"))),
      (hex"0xc80002121234", List(MsgpackItem.Extension(0x12, hex"1234"))),
      (hex"0xc90000000312123456", List(MsgpackItem.Extension(0x12, hex"123456"))),

      // float32, float64
      (hex"0xca3e800000", List(MsgpackItem.Float32(hex"3e800000"))),
      (hex"0xcb3fd0000000000000", List(MsgpackItem.Float64(hex"0x3fd0000000000000"))),

      // uint8, uint16, uint32, uint64
      (hex"0xcc13", List(MsgpackItem.UnsignedInt(hex"13"))),
      (hex"0xcd2137", List(MsgpackItem.UnsignedInt(hex"2137"))),
      (hex"0xce2137abba", List(MsgpackItem.UnsignedInt(hex"2137abba"))),
      (hex"0xcf2137abba12345678", List(MsgpackItem.UnsignedInt(hex"2137abba12345678"))),

      // int8, int16, int32, int64
      (hex"0xd013", List(MsgpackItem.SignedInt(hex"13"))),
      (hex"0xd12137", List(MsgpackItem.SignedInt(hex"2137"))),
      (hex"0xd22137abba", List(MsgpackItem.SignedInt(hex"2137abba"))),
      (hex"0xd32137abba12345678", List(MsgpackItem.SignedInt(hex"2137abba12345678"))),

      // fixext1, fixext2, fixext4, fixext8, fixext16
      (hex"0xd412ab", List(MsgpackItem.Extension(0x12, hex"ab"))),
      (hex"0xd512abcd", List(MsgpackItem.Extension(0x12, hex"abcd"))),

      // str8, str16, str32
      (hex"0xd903616263", List(MsgpackItem.Str(hex"0x616263"))),
      (hex"0xda0003616263", List(MsgpackItem.Str(hex"0x616263"))),
      (hex"0xdb00000003616263", List(MsgpackItem.Str(hex"0x616263"))),

      // array16, array32
      (hex"0xdc2137", List(MsgpackItem.Array(8503))),
      (hex"0xdd2137abba", List(MsgpackItem.Array(557296570))),

      // map16, map32
      (hex"0xde2137", List(MsgpackItem.Map(8503))),
      (hex"0xdf2137abba", List(MsgpackItem.Map(557296570))),

      // negative fixint
      (hex"e9", List(MsgpackItem.SignedInt(hex"e9"))), // -23
      (hex"f7", List(MsgpackItem.SignedInt(hex"f7"))), // -9

      // timestamp
      (hex"0xd6ff12341234", List(MsgpackItem.Timestamp32(0x12341234))),
      (hex"0xd7ff1234123412341234", List(MsgpackItem.Timestamp64(0x1234123412341234L))),
      (hex"0xc70cff123412341234123412341234", List(MsgpackItem.Timestamp96(0x12341234, 0x1234123412341234L)))
    )

    Stream
      .emits(cases)
      .evalMap { case (hex, repr) =>
        Stream
          .chunk(Chunk.byteVector(hex))
          .through(low.items[IO])
          .compile
          .toList
          .attempt
          .map(encoded => expect.same(encoded, Right(repr)))
      }
      .compile
      .foldMonoid
  }
  test("MessagePack value parser should raise when the reserved value is used") {
    Stream
      .chunk(Chunk.byteVector(hex"c1"))
      .through(low.items[IO])
      .attempt
      .map(_.isLeft)
      .map(expect(_))
      .compile
      .foldMonoid
  }

  test("MessagePack value parser should raise when invalid timestamp length is used") {
    val cases = List(
      // fixext
      hex"0xd4ff00", // length 1
      hex"0xd5ff0000", // length 2

      // ext
      hex"0xc701ff00", // length 1
      hex"0xc70dff00000000000000000000000000" // length 13
    )

    Stream
      .emits(cases)
      .evalMap { n =>
        Stream
          .chunk(Chunk.byteVector(n))
          .through(low.items[IO])
          .compile
          .toList
          .attempt
          .map(_.isLeft)
          .map(expect(_))
      }
      .compile
      .foldMonoid
  }
  test("MessagePack value parser should correctly parse example formats") {
    val cases = List(
      // [ 1, 2, 3 ]
      (hex"93 01 02 03",
       List(MsgpackItem.Array(3),
            MsgpackItem.SignedInt(hex"01"),
            MsgpackItem.SignedInt(hex"02"),
            MsgpackItem.SignedInt(hex"03"))),

      /* [
       *   "on",
       *   {
       *     "title": "Window",
       *     "name": "main_window",
       *     "width": 500,
       *     "height": 500
       *   }
       * ]
       */
      (hex"92 A2 6F 6E 84 A5 74 69 74 6C 65 A6 57 69 6E 64 6F 77 A4 6E 61 6D 65 AB 6D 61 69 6E 5F 77 69 6E 64 6F 77 A5 77 69 64 74 68 CD 01 F4 A6 68 65 69 67 68 74 CD 01 F4",
       List(
         MsgpackItem.Array(2),
         MsgpackItem.Str(hex"6f6e"),
         MsgpackItem.Map(4),
         MsgpackItem.Str(hex"7469746C65"),
         MsgpackItem.Str(hex"57696E646F77"),
         MsgpackItem.Str(hex"6E616D65"),
         MsgpackItem.Str(hex"6D61696E5F77696E646F77"),
         MsgpackItem.Str(hex"7769647468"),
         MsgpackItem.UnsignedInt(hex"1f4"),
         MsgpackItem.Str(hex"686569676874"),
         MsgpackItem.UnsignedInt(hex"1f4")
       )),

      /* {
       *    "int": 1,
       *    "float": 0.5,
       *    "boolean": true,
       *    "null": null,
       *    "string": "foo bar",
       *    "array": [
       *      "foo",
       *      "bar"
       *    ],
       *    "object": {
       *      "foo": 1,
       *      "baz": 0.5
       *    },
       *    "timestamp": <9:02:47 pm UTC + 280716ns, August 20, 2414>
       * }
       */
      (hex"""
        88
        A3 69 6E 74
        01
        A5 66 6C 6F 61 74
        CA 3F 00 00 00
        A7 62 6F 6F 6C 65 61 6E
        C3
        A4 6E 75 6C 6C
        C0
        A6 73 74 72 69 6E 67
        A7 66 6F 6F 20 62 61 72
        A5 61 72 72 61 79
        92
        A3 66 6F 6F
        A3 62 61 72
        A6 6F 62 6A 65 63 74
        82
        A3 66 6F 6F
        01
        A3 62 61 7A
        CB 3F E0 00 00 00 00 00 00
        A9 74 69 6D 65 73 74 61 6D 70
        D7 FF 00 11 22 33 44 55 66 77
        """,
       List(
         MsgpackItem.Map(8),
         MsgpackItem.Str(ByteVector("int".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.SignedInt(hex"01"),
         MsgpackItem.Str(ByteVector("float".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.Float32(hex"3f 00 00 00"), // 0.5 single prec.
         MsgpackItem.Str(ByteVector("boolean".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.True,
         MsgpackItem.Str(ByteVector("null".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.Nil,
         MsgpackItem.Str(ByteVector("string".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.Str(ByteVector("foo bar".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.Str(ByteVector("array".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.Array(2),
         MsgpackItem.Str(ByteVector("foo".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.Str(ByteVector("bar".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.Str(ByteVector("object".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.Map(2),
         MsgpackItem.Str(ByteVector("foo".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.SignedInt(hex"01"),
         MsgpackItem.Str(ByteVector("baz".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.Float64(hex"3f e0 00 00 00 00 00 00"), // 0.5 double prec.
         MsgpackItem.Str(ByteVector("timestamp".getBytes(StandardCharsets.UTF_8))),
         MsgpackItem.Timestamp64(0x0011223344556677L) // 9:02:47 pm UTC + 280716ns, August 20, 2414
       ))
    )

    Stream
      .emits(cases)
      .evalMap {
        case (hex, repr) => {
          Stream
            .chunk(Chunk.byteVector(hex))
            .through(low.items[IO])
            .compile
            .toList
            .attempt
            .map(encoded => expect.same(encoded, Right(repr)))
        }
      }
      .compile
      .foldMonoid
  }
}
