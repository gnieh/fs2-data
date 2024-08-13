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

import weaver.SimpleIOSuite
import scodec.bits.*
import low.MsgpackItem
import cats.effect.*
import low.internal.ValidationErrorAt
import low.internal.ValidationError

import java.nio.charset.StandardCharsets

object SerializerSpec extends SimpleIOSuite {
  test("MessagePack item serializer should correctly serialize all formats") {
    val cases = List(
      // nil, false, true
      (List(MsgpackItem.Nil, MsgpackItem.False, MsgpackItem.True), hex"c0c2c3", hex"c0c2c3"),

      // positive fixint
      (List(MsgpackItem.SignedInt(hex"7b")), hex"7b", hex"0xd3000000000000007b"),
      // negative fixint
      (List(MsgpackItem.SignedInt(hex"d6")), hex"d6", hex"0xd300000000000000d6"),

      // uint 8, uint 16, uint 32, uint 64
      (List(MsgpackItem.UnsignedInt(hex"ab")), hex"ccab", hex"cf00000000000000ab"),
      (List(MsgpackItem.UnsignedInt(hex"abcd")), hex"cdabcd", hex"cf000000000000abcd"),
      (List(MsgpackItem.UnsignedInt(hex"abcdef01")), hex"ceabcdef01", hex"cf00000000abcdef01"),
      (List(MsgpackItem.UnsignedInt(hex"abcdef0123456789")), hex"cfabcdef0123456789", hex"cfabcdef0123456789"),

      // int 8, int 16, int 32, int 64
      (List(MsgpackItem.SignedInt(hex"80")), hex"d080", hex"d30000000000000080"),
      (List(MsgpackItem.SignedInt(hex"80ab")), hex"d180ab", hex"d300000000000080ab"),
      (List(MsgpackItem.SignedInt(hex"80abcdef")), hex"d280abcdef", hex"d30000000080abcdef"),
      (List(MsgpackItem.SignedInt(hex"80abcddef0123456")), hex"d380abcddef0123456", hex"d380abcddef0123456"),

      // float 32, float 64
      (List(MsgpackItem.Float32(0.125F)), hex"ca3e000000", hex"ca3e000000"),
      (List(MsgpackItem.Float64(0.125)), hex"cb3fc0000000000000", hex"cb3fc0000000000000"),

      // fixstr
      (List(MsgpackItem.Str(ByteVector("abc".getBytes(StandardCharsets.UTF_8)))),
       hex"a3616263",
       hex"0xdb00000003616263"),

      // str 8
      (List(MsgpackItem.Str(ByteVector("abcd".repeat(8).getBytes(StandardCharsets.UTF_8)))),
       hex"d920" ++ ByteVector("abcd".repeat(8).getBytes(StandardCharsets.UTF_8)),
       hex"db00000020" ++ ByteVector("abcd".repeat(8).getBytes(StandardCharsets.UTF_8))),

      // str 16
      (List(MsgpackItem.Str(ByteVector("a".repeat(Math.pow(2, 8).toInt).getBytes(StandardCharsets.UTF_8)))),
       hex"da0100" ++ ByteVector("a".repeat(Math.pow(2, 8).toInt).getBytes(StandardCharsets.UTF_8)),
       hex"db00000100" ++ ByteVector("a".repeat(Math.pow(2, 8).toInt).getBytes(StandardCharsets.UTF_8))),

      // str 32
      (List(MsgpackItem.Str(ByteVector("a".repeat(Math.pow(2, 16).toInt).getBytes(StandardCharsets.UTF_8)))),
       hex"db00010000" ++ ByteVector("a".repeat(Math.pow(2, 16).toInt).getBytes(StandardCharsets.UTF_8)),
       hex"db00010000" ++ ByteVector("a".repeat(Math.pow(2, 16).toInt).getBytes(StandardCharsets.UTF_8))),

      // bin 8
      (List(MsgpackItem.Bin(ByteVector("abcd".repeat(8).getBytes(StandardCharsets.UTF_8)))),
       hex"c420" ++ ByteVector("abcd".repeat(8).getBytes(StandardCharsets.UTF_8)),
       hex"c600000020" ++ ByteVector("abcd".repeat(8).getBytes(StandardCharsets.UTF_8))),

      // bin 16
      (List(MsgpackItem.Bin(ByteVector("a".repeat(Math.pow(2, 8).toInt).getBytes(StandardCharsets.UTF_8)))),
       hex"c50100" ++ ByteVector("a".repeat(Math.pow(2, 8).toInt).getBytes(StandardCharsets.UTF_8)),
       hex"c600000100" ++ ByteVector("a".repeat(Math.pow(2, 8).toInt).getBytes(StandardCharsets.UTF_8))),

      // bin 32
      (List(MsgpackItem.Bin(ByteVector("a".repeat(Math.pow(2, 16).toInt).getBytes(StandardCharsets.UTF_8)))),
       hex"c600010000" ++ ByteVector("a".repeat(Math.pow(2, 16).toInt).getBytes(StandardCharsets.UTF_8)),
       hex"c600010000" ++ ByteVector("a".repeat(Math.pow(2, 16).toInt).getBytes(StandardCharsets.UTF_8))),

      // fixarray
      (List(MsgpackItem.Array(0)), hex"90", hex"dd00000000"),
      (List(MsgpackItem.Array(1)), hex"91", hex"dd00000001"),
      // array 16
      (List(MsgpackItem.Array(16)), hex"dc0010", hex"dd00000010"),
      // array 32
      (List(MsgpackItem.Array(Math.pow(2, 16).toInt)), hex"dd00010000", hex"dd00010000"),

      // fixmap
      (List(MsgpackItem.Map(0)), hex"80", hex"df00000000"),
      (List(MsgpackItem.Map(1)), hex"81", hex"df00000001"),
      // map 16
      (List(MsgpackItem.Map(16)), hex"de0010", hex"df00000010"),
      // map 32
      (List(MsgpackItem.Map(Math.pow(2, 16).toInt)), hex"df00010000", hex"df00010000"),

      // fixext 1
      (List(MsgpackItem.Extension(0x54.toByte, hex"ab")), hex"d454ab", hex"c90000000154ab"),
      // fixext 2
      (List(MsgpackItem.Extension(0x54.toByte, hex"abcd")), hex"d554abcd", hex"c90000000254abcd"),
      // fixext 4
      (List(MsgpackItem.Extension(0x54.toByte, hex"abcdef01")), hex"d654abcdef01", hex"c90000000454abcdef01"),
      // fixext 8
      (List(MsgpackItem.Extension(0x54.toByte, hex"abcdef0123456789")),
       hex"d754abcdef0123456789",
       hex"c90000000854abcdef0123456789"),
      // fixext 8
      (List(MsgpackItem.Extension(0x54.toByte, hex"abcdef0123456789abcdef0123456789")),
       hex"d854abcdef0123456789abcdef0123456789",
       hex"c90000001054abcdef0123456789abcdef0123456789"),

      // ext 8
      (List(MsgpackItem.Extension(0x54, hex"ab".padLeft(17))),
       hex"c71154" ++ hex"ab".padLeft(17),
       hex"c90000001154" ++ hex"ab".padLeft(17)),

      // ext 16
      (List(MsgpackItem.Extension(0x54, hex"ab".padLeft(Math.pow(2, 8).toLong))),
       hex"c8010054" ++ hex"ab".padLeft(Math.pow(2, 8).toLong),
       hex"c90000010054" ++ hex"ab".padLeft(Math.pow(2, 8).toLong)),

      // ext 32
      (List(MsgpackItem.Extension(0x54, hex"ab".padLeft(Math.pow(2, 16).toLong))),
       hex"c90001000054" ++ hex"ab".padLeft(Math.pow(2, 16).toLong),
       hex"c90001000054" ++ hex"ab".padLeft(Math.pow(2, 16).toLong)),

      // timestamp 32
      (List(MsgpackItem.Timestamp32(0x0123abcd)), hex"d6ff0123abcd", hex"d6ff0123abcd"),

      // timestamp 64
      (List(MsgpackItem.Timestamp64(0x0123456789abcdefL)), hex"d7ff0123456789abcdef", hex"d7ff0123456789abcdef"),

      // timestamp 96
      (List(MsgpackItem.Timestamp96(0x0123abcd, 0x0123456789abcdefL)),
       hex"c70cff0123abcd0123456789abcdef",
       hex"c70cff0123abcd0123456789abcdef")
    )

    Stream
      .emits(cases)
      .evalMap { case (source, compressed, fast) =>
        for {
          e1 <-
            Stream
              .emits(source)
              .through(low.bytes(true, false))
              .compile
              .fold(ByteVector.empty)(_ :+ _)
              .map(expect.same(_, compressed))

          e2 <-
            Stream
              .emits(source)
              .through(low.bytes(false, false))
              .compile
              .fold(ByteVector.empty)(_ :+ _)
              .map(expect.same(_, fast))
        } yield e1 and e2
      }
      .compile
      .foldMonoid
  }

  test("MessagePack item serializer should be fix point when optimizing for size") {
    val cases = List(
      hex"CB3FCB5A858793DD98",
      hex"d6ffaabbccdd",
      hex"81A77765622D61707083A7736572766C65749583AC736572766C65742D6E616D65A8636F666178434453AD736572766C65742D636C617373B86F72672E636F6661782E6364732E434453536572766C6574AA696E69742D706172616DDE002ABD636F6E666967476C6F73736172793A696E7374616C6C6174696F6E4174B05068696C6164656C706869612C205041B9636F6E666967476C6F73736172793A61646D696E456D61696CAD6B736D40706F626F782E636F6DB8636F6E666967476C6F73736172793A706F77657265644279A5436F666178BC636F6E666967476C6F73736172793A706F7765726564427949636F6EB12F696D616765732F636F6661782E676966B9636F6E666967476C6F73736172793A73746174696350617468AF2F636F6E74656E742F737461746963B674656D706C61746550726F636573736F72436C617373B96F72672E636F6661782E5779736977796754656D706C617465B374656D706C6174654C6F61646572436C617373BD6F72672E636F6661782E46696C657354656D706C6174654C6F61646572AC74656D706C61746550617468A974656D706C61746573B474656D706C6174654F7665727269646550617468A0B364656661756C744C69737454656D706C617465B06C69737454656D706C6174652E68746DB364656661756C7446696C6554656D706C617465B361727469636C6554656D706C6174652E68746DA67573654A5350C2AF6A73704C69737454656D706C617465B06C69737454656D706C6174652E6A7370AF6A737046696C6554656D706C617465B361727469636C6554656D706C6174652E6A7370B563616368655061636B61676554616773547261636BCCC8B563616368655061636B6167655461677353746F7265CCC8B763616368655061636B61676554616773526566726573683CB3636163686554656D706C61746573547261636B64B3636163686554656D706C6174657353746F726532B5636163686554656D706C61746573526566726573680FAF63616368655061676573547261636BCCC8AF6361636865506167657353746F726564B163616368655061676573526566726573680AB3636163686550616765734469727479526561640AB8736561726368456E67696E654C69737454656D706C617465B8666F72536561726368456E67696E65734C6973742E68746DB8736561726368456E67696E6546696C6554656D706C617465B4666F72536561726368456E67696E65732E68746DB4736561726368456E67696E65526F626F74734462B15745422D494E462F726F626F74732E6462AC7573654461746153746F7265C3AE6461746153746F7265436C617373B66F72672E636F6661782E53716C4461746153746F7265B07265646972656374696F6E436C617373B86F72672E636F6661782E53716C5265646972656374696F6EAD6461746153746F72654E616D65A5636F666178AF6461746153746F7265447269766572D92C636F6D2E6D6963726F736F66742E6A6462632E73716C7365727665722E53514C536572766572447269766572AC6461746153746F726555726CD93B6A6462633A6D6963726F736F66743A73716C7365727665723A2F2F4C4F43414C484F53543A313433333B44617461626173654E616D653D676F6F6EAD6461746153746F726555736572A27361B16461746153746F726550617373776F7264B26461746153746F7265546573745175657279B26461746153746F7265546573745175657279D922534554204E4F434F554E54204F4E3B73656C65637420746573743D2774657374273BB06461746153746F72654C6F6746696C65D9242F7573722F6C6F63616C2F746F6D6361742F6C6F67732F6461746173746F72652E6C6F67B26461746153746F7265496E6974436F6E6E730AB16461746153746F72654D6178436F6E6E7364B76461746153746F7265436F6E6E55736167654C696D697464B16461746153746F72654C6F674C6576656CA56465627567AC6D617855726C4C656E677468CD01F483AC736572766C65742D6E616D65AA636F666178456D61696CAD736572766C65742D636C617373BA6F72672E636F6661782E6364732E456D61696C536572766C6574AA696E69742D706172616D82A86D61696C486F7374A56D61696C31B06D61696C486F73744F76657272696465A56D61696C3282AC736572766C65742D6E616D65AA636F66617841646D696EAD736572766C65742D636C617373BA6F72672E636F6661782E6364732E41646D696E536572766C657482AC736572766C65742D6E616D65AB66696C65536572766C6574AD736572766C65742D636C617373B96F72672E636F6661782E6364732E46696C65536572766C657483AC736572766C65742D6E616D65AA636F666178546F6F6C73AD736572766C65742D636C617373BF6F72672E636F6661782E636D732E436F666178546F6F6C73536572766C6574AA696E69742D706172616D8DAC74656D706C61746550617468AF746F6F6C7374656D706C617465732FA36C6F6701AB6C6F674C6F636174696F6ED9252F7573722F6C6F63616C2F746F6D6361742F6C6F67732F436F666178546F6F6C732E6C6F67AA6C6F674D617853697A65A0A7646174614C6F6701AF646174614C6F674C6F636174696F6ED9222F7573722F6C6F63616C2F746F6D6361742F6C6F67732F646174614C6F672E6C6F67AE646174614C6F674D617853697A65A0AF72656D6F7665506167654361636865D9252F636F6E74656E742F61646D696E2F72656D6F76653F63616368653D70616765732669643DB372656D6F766554656D706C6174654361636865D9292F636F6E74656E742F61646D696E2F72656D6F76653F63616368653D74656D706C617465732669643DB266696C655472616E73666572466F6C646572D9342F7573722F6C6F63616C2F746F6D6361742F776562617070732F636F6E74656E742F66696C655472616E73666572466F6C646572AD6C6F6F6B496E436F6E7465787401AC61646D696E47726F7570494404AA62657461536572766572C3AF736572766C65742D6D617070696E6785A8636F666178434453A12FAA636F666178456D61696CB32F636F6661787574696C2F61656D61696C2F2AAA636F66617841646D696EA82F61646D696E2F2AAB66696C65536572766C6574A92F7374617469632F2AAA636F666178546F6F6C73A82F746F6F6C732F2AA67461676C696282AA7461676C69622D757269A9636F6661782E746C64AF7461676C69622D6C6F636174696F6EB72F5745422D494E462F746C64732F636F6661782E746C64"
    )

    Stream
      .emits(cases)
      .evalMap { hex =>
        Stream
          .chunk(Chunk.byteVector(hex))
          .through(low.items)
          .through(low.toBinary)
          .compile
          .toList
          .map(x => expect.same(ByteVector(x), hex))
      }
      .compile
      .foldMonoid
  }

  test("MessagePack item validator should raise for all checks") {
    val cases = List(
      List(MsgpackItem.UnsignedInt(hex"10000000000000000")) -> new ValidationErrorAt(0, "Unsigned int exceeds 64 bits"),
      List(MsgpackItem.SignedInt(hex"10000000000000000")) -> new ValidationErrorAt(0, "Signed int exceeds 64 bits"),

      // TODO: Float32, Float64

      List(MsgpackItem.Str(ByteVector.fill(Math.pow(2, 32).toLong)(1))) -> new ValidationErrorAt(
        0,
        "String exceeds (2^32)-1 bytes"),
      List(MsgpackItem.Bin(ByteVector.fill(Math.pow(2, 32).toLong)(1))) -> new ValidationErrorAt(
        0,
        "Bin exceeds (2^32)-1 bytes"),
      List(MsgpackItem.Array(2), MsgpackItem.True) -> new ValidationError("Unexpected end of input (starting at 0)"),
      List(MsgpackItem.Map(1), MsgpackItem.Array(1), MsgpackItem.True) -> new ValidationError(
        "Unexpected end of input (starting at 0)")
    )

    Stream
      .emits(cases)
      .evalMap { case (lhs, rhs) =>
        Stream
          .emits(lhs)
          .through(low.validated[IO])
          .compile
          .toList
          .map(x => failure(s"Expected error for item ${x}"))
          .handleErrorWith(err => IO(expect.same(err, rhs)))
      }
      .compile
      .foldMonoid
  }
}
