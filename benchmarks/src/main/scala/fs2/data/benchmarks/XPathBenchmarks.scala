package fs2
package data.benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import cats.effect.IO
import cats.effect.unsafe.implicits._
import fs2.data.xml.xpath._
import literals._
import fs2.data.xml.xpath.internals.XmlQueryPipe
import fs2.data.xml.XmlEvent

/* Default settings for benchmarks in this class */
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(org.openjdk.jmh.annotations.Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 5, time = 2)
@Measurement(iterations = 10, time = 2)
class XPathBenchmarks {

  val xmlStream: Stream[IO, XmlEvent] =
    Stream
      .emit("""<a>
              |  <b>
              |    <c>
              |      <a>
              |        <c />
              |      </a>
              |    </c>
              |  </b>
              |</a>
              |<root><a><c><a>text</a></c></a></root>""".stripMargin)
      .through(fs2.data.xml.events[IO, String]())
      .compile
      .toList
      .map(Stream.emits(_))
      .unsafeRunSync()

  val pipe = new XmlQueryPipe[IO](compileXPath(xpath"//c/*"))

  @Benchmark
  def raw(): Unit = {
    xmlStream
      .through(pipe.raw(Int.MaxValue, Int.MaxValue))
      .parEvalMapUnbounded(_.compile.drain)
      .compile
      .drain
      .unsafeRunSync()
  }

}
