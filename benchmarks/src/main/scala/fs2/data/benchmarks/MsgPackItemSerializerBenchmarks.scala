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

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(org.openjdk.jmh.annotations.Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 10, time = 2)
class MsgPackItemSerializerBenchmarks {
  val msgpackItems: Stream[SyncIO, fs2.data.msgpack.low.MsgpackItem] =
    fs2.io
      .readClassLoaderResource[SyncIO]("twitter_msgpack.mp", 4096)
      .through(fs2.data.msgpack.low.items[SyncIO])
      .chunks
      .compile
      .toList
      .unsafeRunSync()
      .map(Stream.chunk)
      .fold(Stream.empty)(_ ++ _)

  @Benchmark
  def serialize() =
    msgpackItems
      .through(fs2.data.msgpack.low.toNonValidatedBinary[SyncIO])
      .compile
      .drain
      .unsafeRunSync()

  @Benchmark
  def withValidation() =
    msgpackItems
      .through(fs2.data.msgpack.low.toBinary[SyncIO])
      .compile
      .drain
      .unsafeRunSync()
}
