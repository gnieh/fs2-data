package fs2
package data.benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import cats.effect.IO
import cats.effect.unsafe.implicits._
import fs2.data.xml.scalaXml._
import scala.xml.InputSource
import scala.xml.XML

/* Default settings for benchmarks in this class */
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(org.openjdk.jmh.annotations.Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
class XmlParserBenchmarks {

  def xmlStream: Stream[IO, Byte] =
    fs2.io.readClassLoaderResource[IO]("benchmark.xml", 4096)

  @Benchmark
  def parseFs2Data(): Unit = {
    xmlStream
      .through(fs2.text.utf8.decode)
      .through(fs2.data.xml.events())
      .through(fs2.data.xml.dom.documents)
      .compile
      .lastOrError
      .unsafeRunSync()
  }

  @Benchmark
  def parseScalaXml(): Unit = {
    xmlStream
      .through(fs2.io.toInputStream)
      .evalMap(in => IO.blocking {
        val source = new InputSource()
        source.setByteStream(in)
        XML.loadXML(source, XML.parser)
      })
      .compile
      .lastOrError
      .unsafeRunSync()
  }
}
