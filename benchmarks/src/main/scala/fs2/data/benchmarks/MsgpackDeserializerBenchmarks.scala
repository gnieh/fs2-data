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
package data.benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import cats.effect.SyncIO

import fs2.data.msgpack.high._
import fs2.data.msgpack.low.MsgpackItem
import fs2.data.msgpack.low
import java.time.Instant
import scodec.bits._

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(org.openjdk.jmh.annotations.Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 10, time = 2)
class MsgpackDeserializerBenchmarks {
  case class User(name: String, age: Long, aliases: List[String], balance: Double, things: Map[String, Int], raw: ByteVector, created: Instant)

  implicit val userDecoder: MsgpackDeserializer[User] = for {
    name <- deserializer[String]
    age <- deserializer[Long]
    aliases <- deserializer[List[String]]
    balance <- deserializer[Double]
    things <- deserializer[Map[String, Int]]
    raw <- deserializer[ByteVector]
    created <- deserializer[Instant]
  } yield User(name, age, aliases, balance, things, raw, created)

  def getChunkedItems(filename: String): List[Chunk[MsgpackItem]] =
    fs2.io
      .readClassLoaderResource[SyncIO](filename, 4096)
      .through(low.items)
      .chunks
      .compile
      .toList
      .unsafeRunSync()

  var userStream: Stream[Pure, MsgpackItem] = _

  @Setup
  def readUserItems() = userStream = Stream.emits(getChunkedItems("users.mp")).unchunks

  @Benchmark
  def deserialize() =
    userStream
      .through(fs2.data.msgpack.high.fromItems[SyncIO, User])
      .compile
      .drain
      .unsafeRunSync()

  @Benchmark
  def deserializeValues() =
    userStream
      .through(fs2.data.msgpack.high.ast.valuesFromItems[SyncIO])
      .compile
      .drain
      .unsafeRunSync()
}
