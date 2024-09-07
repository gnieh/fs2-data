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
import scodec.bits._
import weaver._

import java.nio.charset.StandardCharsets
import low.MsgpackItem

object SerializerSpec extends SimpleIOSuite {
  test("MessagePack item serializer should correctly serialize all formats") {
    val cases: List[(List[MsgpackItem], ByteVector)] = List(
      // nil, false, true
      (List(MsgpackItem.Nil, MsgpackItem.False, MsgpackItem.True), hex"c0c2c3"),

      // positive fixint
      (List(MsgpackItem.SignedInt(hex"7b")), hex"7b"),
      // negative fixint
      (List(MsgpackItem.SignedInt(hex"d6")), hex"d6"),

      // uint 8, uint 16, uint 32, uint 64
      (List(MsgpackItem.UnsignedInt(hex"ab")), hex"ccab"),
      (List(MsgpackItem.UnsignedInt(hex"abcd")), hex"cdabcd"),
      (List(MsgpackItem.UnsignedInt(hex"abcdef01")), hex"ceabcdef01"),
      (List(MsgpackItem.UnsignedInt(hex"abcdef0123456789")), hex"cfabcdef0123456789"),

      // int 8, int 16, int 32, int 64
      (List(MsgpackItem.SignedInt(hex"80")), hex"d080"),
      (List(MsgpackItem.SignedInt(hex"80ab")), hex"d180ab"),
      (List(MsgpackItem.SignedInt(hex"80abcdef")), hex"d280abcdef"),
      (List(MsgpackItem.SignedInt(hex"80abcddef0123456")), hex"d380abcddef0123456"),

      // float 32, float 64
      (List(MsgpackItem.Float32(0.125F)), hex"ca3e000000"),
      (List(MsgpackItem.Float64(0.125)), hex"cb3fc0000000000000"),

      // fixstr
      (List(MsgpackItem.Str(ByteVector("abc".getBytes(StandardCharsets.UTF_8)))), hex"a3616263"),

      // str 8
      (List(MsgpackItem.Str(ByteVector("abcd".repeat(8).getBytes(StandardCharsets.UTF_8)))),
       hex"d920" ++ ByteVector("abcd".repeat(8).getBytes(StandardCharsets.UTF_8))),

      // str 16
      (List(MsgpackItem.Str(ByteVector("a".repeat(Math.pow(2, 8).toInt).getBytes(StandardCharsets.UTF_8)))),
       hex"da0100" ++ ByteVector("a".repeat(Math.pow(2, 8).toInt).getBytes(StandardCharsets.UTF_8))),

      // str 32
      (List(MsgpackItem.Str(ByteVector("a".repeat(Math.pow(2, 16).toInt).getBytes(StandardCharsets.UTF_8)))),
       hex"db00010000" ++ ByteVector("a".repeat(Math.pow(2, 16).toInt).getBytes(StandardCharsets.UTF_8))),

      // bin 8
      (List(MsgpackItem.Bin(ByteVector("abcd".repeat(8).getBytes(StandardCharsets.UTF_8)))),
       hex"c420" ++ ByteVector("abcd".repeat(8).getBytes(StandardCharsets.UTF_8))),

      // bin 16
      (List(MsgpackItem.Bin(ByteVector("a".repeat(Math.pow(2, 8).toInt).getBytes(StandardCharsets.UTF_8)))),
       hex"c50100" ++ ByteVector("a".repeat(Math.pow(2, 8).toInt).getBytes(StandardCharsets.UTF_8))),

      // bin 32
      (List(MsgpackItem.Bin(ByteVector("a".repeat(Math.pow(2, 16).toInt).getBytes(StandardCharsets.UTF_8)))),
       hex"c600010000" ++ ByteVector("a".repeat(Math.pow(2, 16).toInt).getBytes(StandardCharsets.UTF_8))),

      // fixarray
      (List(MsgpackItem.Array(0)), hex"90"),
      (List(MsgpackItem.Array(1)), hex"91"),
      // array 16
      (List(MsgpackItem.Array(16)), hex"dc0010"),
      // array 32
      (List(MsgpackItem.Array(Math.pow(2, 16).toInt)), hex"dd00010000"),

      // fixmap
      (List(MsgpackItem.Map(0)), hex"80"),
      (List(MsgpackItem.Map(1)), hex"81"),
      // map 16
      (List(MsgpackItem.Map(16)), hex"de0010"),
      // map 32
      (List(MsgpackItem.Map(Math.pow(2, 16).toInt)), hex"df00010000"),

      // fixext 1
      (List(MsgpackItem.Extension(0x54.toByte, hex"ab")), hex"d454ab"),
      // fixext 2
      (List(MsgpackItem.Extension(0x54.toByte, hex"abcd")), hex"d554abcd"),
      // fixext 4
      (List(MsgpackItem.Extension(0x54.toByte, hex"abcdef01")), hex"d654abcdef01"),
      // fixext 8
      (List(MsgpackItem.Extension(0x54.toByte, hex"abcdef0123456789")), hex"d754abcdef0123456789"),
      // fixext 8
      (List(MsgpackItem.Extension(0x54.toByte, hex"abcdef0123456789abcdef0123456789")),
       hex"d854abcdef0123456789abcdef0123456789"),

      // ext 8
      (List(MsgpackItem.Extension(0x54, ByteVector.fill(17)(0xab))), hex"c71154" ++ ByteVector.fill(17)(0xab)),

      // ext 16
      (List(MsgpackItem.Extension(0x54, ByteVector.fill(Math.pow(2, 8).toLong)(0xab))),
       hex"c8010054" ++ ByteVector.fill(Math.pow(2, 8).toLong)(0xab)),

      // ext 32
      (List(MsgpackItem.Extension(0x54, ByteVector.fill(Math.pow(2, 16).toLong)(0xab))),
       hex"c90001000054" ++ ByteVector.fill(Math.pow(2, 16).toLong)(0xab)),

      // timestamp 32
      (List(MsgpackItem.Timestamp32(0x0123abcd)), hex"d6ff0123abcd"),

      // timestamp 64
      (List(MsgpackItem.Timestamp64(0x0123456789abcdefL)), hex"d7ff0123456789abcdef"),

      // timestamp 96
      (List(MsgpackItem.Timestamp96(0x0123abcd, 0x0123456789abcdefL)), hex"c70cff0123abcd0123456789abcdef")
    )

    Stream
      .emits(cases)
      .evalMap { case (source, serialized) =>
        Stream
          .emits(source)
          .through(low.toNonValidatedBinary)
          .compile
          .fold(ByteVector.empty)(_ :+ _)
          .map(expect.same(_, serialized))

      }
      .compile
      .foldMonoid
  }

  test("MessagePack item serializer should be fixpoint for a subset of ByteVector") {
    /* The parser mapping ByteVector to MsgpackItem can be seen as a not injective morphism, that is, there
     * are many ByteVectors that will map to the same MsgpackItem. Because of this, we cannot possibly guarantee that
     * `serialize(parse(bs))` is fixpoint for an arbitrary `bs`. However, currently implemented serializer *is*
     * injective (if we exclude the Timestamp format family as it can be represented with Extension types) and so, we
     * can guarantee `serialize(parse(bs)) == bs` if `bs` is a member of a subset of ByteVector that is emitted by a
     * serializer.
     *
     * In other words, the following code will be true for any `bs` if `serialize` is injective and we ignore the
     * Timestamp type family:
     * {{{
     *  val first = serialize(parse(bs))
     *  val second = serialize(parse(first))
     *  first == second
     * }}}
     *
     * This test makes sure that the above holds.
     */

    val cases = List(
      hex"918FA46461746582A662756666657282A474797065A6427566666572A4646174619401234567A474797065CCFFA35F6964B8363663316233363661333137353434376163346335343165A5696E64657800A467756964D92438666665653537302D353938312D346630362D623635382D653435383163363064373539A86973416374697665C3A762616C616E6365CB40A946956A97C84CA361676516A8657965436F6C6F72A4626C7565A46E616D65AD4D6F72746F6E204C6974746C65A761646472657373D9313933372044656172626F726E20436F7572742C204861726C656967682C204D6173736163687573657474732C2033353936AA72656769737465726564BA323032332D30382D32395431303A34353A3335202D30323A3030A86C61746974756465CB4047551159C49774A96C6F6E676974756465CBC065F94A771C970FA47461677397A54C6F72656DA3657374A86465736572756E74A54C6F72656DA46E697369A76C61626F726973A86465736572756E74A7667269656E64739382A2696400A46E616D65B04865726E616E64657A204C6172736F6E82A2696401A46E616D65AF4D616E6E696E672053617267656E7482A2696402A46E616D65AF536176616E6E6168204E65776D616E"
    )

    def round(data: ByteVector) =
      Stream
        .chunk(Chunk.byteVector(data))
        .through(low.items[IO])
        .through(low.toNonValidatedBinary)
        .fold(ByteVector.empty)(_ :+ _)

    val out = for {
      data <- Stream.emits(cases)
      pre <- round(data)
      processed <- round(pre)
    } yield {
      if (processed == pre)
        success
      else
        failure(s"Serializer should be fixpoint for ${pre} but it emitted ${processed}")
    }

    out.compile.foldMonoid

  }
}
