package fs2
package data
package json

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

/* Default settings for benchmarks in this class */
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(org.openjdk.jmh.annotations.Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 15, time = 1)
@Measurement(iterations = 10, time = 1)
class JsonValueBenchmarks {

  val simpleString = Stream.emit[Fallible, String](""""string with no escape"""")
  val escapedString = Stream.emit[Fallible, String](""""string\nwith\nsome\nescapes"""")

  @Benchmark
  def parseSimpleString() =
    simpleString
      .through(tokens)
      .compile
      .drain

  @Benchmark
  def parseEscapedString() =
    escapedString
      .through(tokens)
      .compile
      .drain

}
