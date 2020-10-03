package fs2
package data.benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import better.files.File
import cats.effect.SyncIO

/* Default settings for benchmarks in this class */
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(org.openjdk.jmh.annotations.Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
class CsvParserBenchmarks {

  var csvContent: List[String] = _

  @Setup
  def readCsv(): Unit = csvContent = {
    File("resources/benchmark.csv").contentAsString.grouped(4096).toList
  }

  def csvStream: Stream[SyncIO, String] = Stream.emits(csvContent).covary[SyncIO]

  @Benchmark
  def parseRows(): Unit = {
    csvStream
      .through(fs2.data.csv.rows[SyncIO, String](','))
      .through(fs2.data.csv.skipHeaders)
      .compile
      .lastOrError
      .unsafeRunSync()
  }

  @Benchmark
  def parseRowsBaseline(): Unit = {
    csvStream.compile.lastOrError.unsafeRunSync()
  }
}
