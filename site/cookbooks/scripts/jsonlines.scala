//> using toolkit typelevel:default
//> using dep org.gnieh::fs2-data-json-circe::1.12.0

import cats.effect.{ExitCode, IO, IOApp}
import com.monovore.decline.{Command, Opts}
import com.monovore.decline.effect.CommandIOApp
import fs2.Stream
import fs2.data.json.JsonException
import fs2.data.json.circe._
import fs2.data.json.jsonpath.literals._
import fs2.io.file.{Files, Path}
import io.circe.Json

sealed trait Opt
object Opt {
  case class Read(input: Path) extends Opt
  case class Produce(input: Path) extends Opt
}

object JsonLines
    extends CommandIOApp(name = "fs2-jsonelines",
                         header = "A simple example CLI tool to read and write JSON Lines data") {

  def readJsonLines(input: Stream[IO, Byte]): Stream[IO, Json] =
    input
      // rule #1: input must be UTF-8 encoded
      .through(fs2.text.utf8.decode)
      // rule #3: new line delimiter is '\n'
      .through(fs2.text.lines)
      .flatMap { line =>
        // rule #2: values must be encoded on single lines
        Stream
          .emit(line)
          .covary[IO]
          .through(fs2.data.json.ast.parse)
          .handleErrorWith { t =>
            Stream.raiseError[IO](JsonException(s"'$line' is not a valid JSON value", inner = t))
          }
      }

  def writeJsonLines(input: Stream[IO, Json]): Stream[IO, Byte] =
    input
      .map { data =>
        // rule #2: values must be encoded on single lines
        Stream
          .emit(data)
          .through(fs2.data.json.ast.tokenize)
          .through(fs2.data.json.render.compact)
          .compile
          .string
      }
      // rule #3: new line delimiter is '\n'
      .intersperse("\n")
      // rule #1: input must be UTF-8 encoded
      .through(fs2.text.utf8.encode)

  val read: Opts[Opt] = Opts.subcommand(
    Command(name = "read", header = "Read a JSON lines formatted file, and print data in a pretty printed JSON array") {
      Opts.argument[String]("file.jsonl").map(jp => Opt.Read(Path(jp)))
    })

  val write: Opts[Opt] =
    Opts.subcommand(Command(name = "produce", header = "Read a JSON array, and print data in a JSON lines format") {
      Opts.argument[String]("file.json").map(jp => Opt.Produce(Path(jp)))
    })

  override def main: Opts[IO[ExitCode]] =
    read.orElse(write).map {
      case Opt.Read(input) =>
        Files[IO]
          .readAll(input)
          .through(readJsonLines)
          .through(fs2.data.json.ast.tokenize)
          .through(fs2.data.json.wrap.asTopLevelArray)
          .through(fs2.data.json.render.prettyPrint())
          .through(fs2.text.utf8.encode)
          .through(fs2.io.stdout)
          .compile
          .drain
          .as(ExitCode.Success)
      case Opt.Produce(input) =>
        Files[IO]
          .readAll(input)
          .through(fs2.text.utf8.decode)
          .through(fs2.data.json.tokens)
          .through(fs2.data.json.jsonpath.filter.values(jsonpath"$$[*]"))
          .through(writeJsonLines)
          .through(fs2.io.stdout)
          .compile
          .drain
          .as(ExitCode.Success)
    }

}
