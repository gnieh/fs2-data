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
import cats.Show
import org.scalacheck.Arbitrary
import scodec.bits._
import weaver._
import weaver.scalacheck._

import cats.syntax.traverse._
import org.scalacheck.Gen

object RoundtripTest extends SimpleIOSuite with Checkers {
  // With implicit generator
  def ride[A: Show: Arbitrary: MsgpackSerializer: MsgpackDeserializer]: IO[Expectations] =
    forall { (x: A) =>
      Stream
        .emit(x)
        .through(high.serialize[IO, A])
        .through(high.deserialize[IO, A])
        .compile
        .lastOrError
        .attempt
        .map(expect.same(_, Right(x)))
    }

  // With explicit egenerator
  def ride[A](
      gen: Gen[A])(implicit S: Show[A], sa: MsgpackSerializer[A], da: MsgpackDeserializer[A]): IO[Expectations] =
    forall(gen) { (x: A) =>
      Stream
        .emit(x)
        .through(high.serialize[IO, A])
        .through(high.deserialize[IO, A])
        .compile
        .lastOrError
        .attempt
        .map(expect.same(_, Right(x)))
    }

  test("deserialization should be an inverse of serialization for a subset of values") {
    List(
      ride[Byte],
      ride[Short],
      ride[Int],
      ride[Long],
      ride[Float],
      ride[Double],
      ride[BigInt](Gen.choose(BigInt(Long.MinValue), BigInt(Long.MaxValue) * 2 - 1)), // max 64bit uint
      ride[String],
      ride[Boolean],

      ride[List[String]],
      ride[Map[Int, Boolean]],
      ride[Option[Int]],
      ride[Either[String, Int]]
    ).traverse(identity).map(_.reduce(_ && _))
  }
}
