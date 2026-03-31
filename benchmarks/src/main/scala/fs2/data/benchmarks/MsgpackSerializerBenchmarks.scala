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
import fs2.data.msgpack.high.ast._
import java.time.Instant
import scodec.bits.ByteVector

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(org.openjdk.jmh.annotations.Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 10, time = 2)
class MsgpackSerializerBenchmarks {
  case class User(name: String,
                  age: Long,
                  aliases: List[String],
                  balance: Double,
                  things: Map[String, Int],
                  raw: ByteVector,
                  created: Instant)

  implicit val userDecoder: MsgpackDeserializer[User] = for {
    name <- MsgpackDeserializer[String]
    age <- MsgpackDeserializer[Long]
    aliases <- MsgpackDeserializer[List[String]]
    balance <- MsgpackDeserializer[Double]
    things <- MsgpackDeserializer[Map[String, Int]]
    raw <- MsgpackDeserializer[ByteVector]
    created <- MsgpackDeserializer[Instant]
  } yield User(name, age, aliases, balance, things, raw, created)

  var objects: Stream[SyncIO, User] = _
  var values: Stream[SyncIO, MsgpackValue] = _

  implicit val userSerializer: MsgpackSerializer[User] = user =>
    for {
      name <- user.name.serialize
      age <- user.age.serialize
      aliases <- user.aliases.serialize
      balance <- user.balance.serialize
      things <- user.things.serialize
      raw <- user.raw.serialize
      created <- user.created.serialize
    } yield name ++ age ++ aliases ++ balance ++ things ++ raw ++ created

  @Setup
  def setupObjects() =
    objects =
      fs2.io
      .readClassLoaderResource[SyncIO]("users.mp", 4096)
      .through(fs2.data.msgpack.high.deserialize[SyncIO, User])
      .compile
      .toList
      .map(Stream.emits)
      .unsafeRunSync()

  @Setup
  def setupValues() =
    values =
      fs2.io
      .readClassLoaderResource[SyncIO]("users.mp", 4096)
      .through(fs2.data.msgpack.high.deserialize[SyncIO, MsgpackValue])
      .compile
      .toList
      .map(Stream.emits)
      .unsafeRunSync()

  @Benchmark
  def serialize() =
    objects
      .through(fs2.data.msgpack.high.toItems[SyncIO, User])
      .compile
      .drain
      .unsafeRunSync()

  @Benchmark
  def serializeValues() =
    values
      .through(fs2.data.msgpack.high.ast.valuesToItems[SyncIO])
      .compile
      .drain
      .unsafeRunSync()

}
