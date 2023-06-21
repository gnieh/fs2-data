package fs2
package data
package json

import cats.effect.SyncIO
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

import circe._

/* Default settings for benchmarks in this class */
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(org.openjdk.jmh.annotations.Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 15, time = 2)
@Measurement(iterations = 10, time = 2)
class JsonParserBenchmarks {

  // make sure I/Os are not accounted for in the benchmark
  val jsonString: String =
    fs2.io
      .readClassLoaderResource[SyncIO]("twitter.json", 4096)
      .through(fs2.text.utf8.decode)
      .compile
      .string
      .unsafeRunSync()

  val jsonStream = Stream.emit[SyncIO, String](jsonString)

  @Benchmark
  def parseJsonFs2DataTokens() =
    jsonStream
      .through(tokens)
      .compile
      .drain
      .unsafeRunSync()

  @Benchmark
  def parseJsonFs2DataValues() =
    jsonStream
      .through(tokens)
      .through(ast.values)
      .compile
      .drain
      .unsafeRunSync()

  @Benchmark
  def parseJsonJawn() =
    _root_.io.circe.jawn.parse(jsonString)

  @Benchmark
  def parseCirceFs2() =
    jsonStream
      .through(_root_.io.circe.fs2.stringStreamParser)
      .compile
      .drain
      .unsafeRunSync()

}
