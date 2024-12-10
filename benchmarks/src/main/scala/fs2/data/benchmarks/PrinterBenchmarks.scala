package fs2.data.benchmarks

import cats.effect.SyncIO
import cats.effect.IO
import fs2.data.json.Token
import fs2.data.json.circe.*
import fs2.{Fallible, Stream}
import io.circe.Json
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import cats.effect.unsafe.implicits.global

import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(org.openjdk.jmh.annotations.Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 15, time = 5)
@Measurement(iterations = 10, time = 5)
class PrinterBenchmarks {

  val intArrayStream =
    Stream.emits(
      Token.StartArray ::
        (List
          .range(0, 1000000)
          .map(i => Token.NumberValue(i.toString())) :+ Token.EndArray))

  val objectStream =
    Stream.emits(
      Token.StartObject ::
        (List
          .range(0, 1000000)
          .flatMap(i => List(Token.Key(s"key:$i"), Token.NumberValue(i.toString()))) :+ Token.EndObject))

  @Benchmark
  def intArrayCompact(bh: Blackhole) =
    bh.consume(
      intArrayStream
        .through(fs2.data.json.render.compact)
        .compile
        .drain)

  @Benchmark
  def objectCompact(bh: Blackhole) =
    bh.consume(
      objectStream
        .through(fs2.data.json.render.compact)
        .compile
        .drain)

  @Benchmark
  def intArrayPretty(bh: Blackhole) =
    bh.consume(
      intArrayStream
        .through(fs2.data.json.render.prettyPrint())
        .compile
        .drain)

  @Benchmark
  def objectPretty(bh: Blackhole) =
    bh.consume(
      objectStream
        .through(fs2.data.json.render.prettyPrint())
        .compile
        .drain)

  @Benchmark
  def intArrayPrettyLegacy(bh: Blackhole) =
    bh.consume(
      intArrayStream
        .through(fs2.data.json.render.pretty())
        .compile
        .drain)

  @Benchmark
  def objectPrettyLegacy(bh: Blackhole) =
    bh.consume(
      objectStream
        .through(fs2.data.json.render.pretty())
        .compile
        .drain)

}
