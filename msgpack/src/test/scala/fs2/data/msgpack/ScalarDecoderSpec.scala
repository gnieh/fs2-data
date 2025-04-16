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
import high.static._

import cats.effect.IO
import cats.Show
import org.scalacheck.Arbitrary
import scodec.bits._
import weaver._
import weaver.scalacheck._

object ScalarDecoderSpec extends SimpleIOSuite with Checkers {
  def testRoundtrip[A: MsgpackDecoder: Arbitrary: Show](name: String)(lift: A => MsgpackItem) =
    test(name) {
      forall { (x: A) =>
        Stream(lift(x))
          .through(fromItems[IO, A])
          .map(got => expect(got == x))
          .compile
          .foldMonoid
      }
    }

  testRoundtrip("long roundtrip") { (x: Long) =>
    MsgpackItem.SignedInt(ByteVector.fromLong(x))
  }

  testRoundtrip("int roundtrip") { (x: Int) =>
    MsgpackItem.SignedInt(ByteVector.fromInt(x))
  }

  testRoundtrip("short roundtrip") { (x: Short) =>
    MsgpackItem.SignedInt(ByteVector.fromShort(x))
  }

  testRoundtrip("byte roundtrip") { (x: Byte) =>
    MsgpackItem.SignedInt(ByteVector.fromByte(x))
  }

  testRoundtrip("float roundtrip") {
    MsgpackItem.Float32(_)
  }

  testRoundtrip("double roundtrip") {
    MsgpackItem.Float64(_)
  }

  testRoundtrip("string roundtrip") { (x: String) =>
    val bytes = ByteVector.encodeString(x)(java.nio.charset.Charset.forName("UTF-8"))

    bytes match {
      case Right(bs) => MsgpackItem.Str(bs)
      case Left(e)   => throw e // this should never happen
    }
  }

  test("integer types should fail when uint exceeds their MaxValues") {
    val long = (decoder[Long],
                MsgpackItem.UnsignedInt(hex"80 00 00 00 00 00 00 00"),
                new MsgpackDecodingTypeMismatchException("uint bigger than Long.MaxValue", "Long"))
    val int = (decoder[Int],
               MsgpackItem.UnsignedInt(hex"80 00 00 00"),
               new MsgpackDecodingTypeMismatchException("uint bigger than Int.MaxValue", "Int"))
    val short = (decoder[Short],
                 MsgpackItem.UnsignedInt(hex"80 00"),
                 new MsgpackDecodingTypeMismatchException("uint bigger than Short.MaxValue", "Short"))
    val byte = (decoder[Byte],
                MsgpackItem.UnsignedInt(hex"80"),
                new MsgpackDecodingTypeMismatchException(s"uint bigger than Byte.MaxValue", "Byte"))

    def f[A](
        triplet: (MsgpackDecoder[A], MsgpackItem, MsgpackDecodingTypeMismatchException)): Stream[IO, Expectations] =
      Stream
        .emit(triplet)
        .evalMap { case (d, in, expected) =>
          Stream[IO, MsgpackItem](in)
            .through(fromItems(d))
            .compile
            .drain
            .redeem(expect.same(_, expected), _ => failure(s"Expected error for item ${expected}"))
        }

    (f(long) ++ f(int) ++ f(short) ++ f(byte)).compile.foldMonoid
  }
}
