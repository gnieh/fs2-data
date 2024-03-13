# Handling JSON Lines data

The [JSON Lines format][jsonlines] describes a way to represent JSON records on single lines. This allows to process records one at a time, reading them as they come.
In this cookbook, we will demonstrate how such data can be read and produced using `fs2-data`.

## Reading JSON Lines data

The `fs2-data` [JSON module][json] can natively read concatenated JSON values in an input stream This means that we can naively read data from the input stream and we will get the token stream out of it.

However, this way, we will not check that the input is actually respecting the JSON lines format. The format actually only has a few rules:
 - Input must be UTF-8 encoded.
 - Each line is a valid JSON value.
 - Lines are separated by `\n`.

We can leverage the operators provided by `fs2` and `fs2-data` to enforce these constraints when reading data.

```scala mdoc
import cats.effect.unsafe.implicits.global

import cats.effect.IO
import fs2.Stream
import fs2.data.json.JsonException
import fs2.data.json.circe._
import fs2.io.file.{Files, Path}
import io.circe.Json

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

```

Using this function, we can read a [JSON Lines data file][data-jsonl] and wrap the elements in an array.

```scala mdoc
val array =
  Files[IO]
    .readAll(Path("site/cookbooks/data/jsonl/nested.jsonl"))
    .through(readJsonLines)
    .through(fs2.data.json.ast.tokenize)
    .through(fs2.data.json.wrap.asTopLevelArray)
    .through(fs2.data.json.render.pretty())

array
  .compile
  .string
  .unsafeRunSync()
```

This reading function will fail if the input data is not JSON Lines encoded.

```scala mdoc
array
  .through(fs2.text.utf8.encode)
  .through(readJsonLines)
  .compile
  .drain
  .attempt
  .unsafeRunSync()
```

## Producing JSON Lines data

Similarly, using `fs2` and `fs2-data` operators, we can generate a stream that will emit each record on a single line.

```scala mdoc
def writeJsonLines(input: Stream[IO, Json]): Stream[IO, Byte] =
  input
    .chunkLimit(1)
    .unchunks
    .flatMap { data =>
      // rule #2: values must be encoded on single lines
      Stream.emit(data).through(fs2.data.json.ast.tokenize).through(fs2.data.json.render.compact)
    }
    // rule #3: new line delimiter is '\n'
    .intersperse("\n")
    // rule #1: input must be UTF-8 encoded
    .through(fs2.text.utf8.encode)
```

Using this function, we can generate JSON Lines encoded data out of a [sample JSON array][data-json]

```scala mdoc
import fs2.data.json.jsonpath.literals._

Files[IO]
  .readAll(Path("site/cookbooks/data/json/sample.json"))
  .through(fs2.text.utf8.decode)
  .through(fs2.data.json.tokens)
  .through(fs2.data.json.jsonpath.filter.values(jsonpath"$$[*]"))
  .take(5)
  .through(writeJsonLines)
  .through(fs2.text.utf8.decode)
  .compile
  .string
  .unsafeRunSync()
```

## Running the full example

The full code can be found in the repository as a [Scala CLI][scala-cli] [script][jsonlines-script].
This example uses [decline][decline] to parse the CLI options.

@:select(platform)

@:choice(jvm)
```shell
$ scala-cli site/cookbooks/scripts/jsonlines.scala -- read site/cookbooks/data/jsonl/nested.jsonl
```

@:choice(js)
```shell
$ scala-cli --js site/cookbooks/scripts/jsonlines.scala -- read site/cookbooks/data/jsonl/nested.jsonl
```

@:choice(native)
```shell
$ scala-cli --native site/cookbooks/scripts/jsonlines.scala -- read site/cookbooks/data/jsonl/nested.jsonl
```

@:@

[jsonlines]: https://jsonlines.org/
[json]: /documentation/json/index.md
[data-jsonl]: /cookbooks/data/jsonl/nested.jsonl
[data-json]: /cookbooks/data/json/sample.json
[scala-cli]: https://scala-cli.virtuslab.org/
[jsonlines-script]: /cookbooks/scripts/jsonlines.scala
[decline]: https://ben.kirw.in/decline/
