# Reading/transforming/writing JSON data

In this cookbook we will demonstrate an example of how the JSON tools provided by `fs2-data` can be used to build a mini `jq`-like CLI tool.

## High-level overview

The general approach to reading/parsing/transforming/generating data with `fs2-data` can be summarized as follows:

```mermaid
graph LR
Reading(Reading) --> Parsing --> Transforming --> Printing --> Writing(Writing)
```

The _Reading_ and _Writing_ steps are not specific to `fs2-data` but rely on pure `fs2` operators or other compatible libraries. The _Parsing_, _Transforming_, and _Printing_ phases will use the tools provided by `fs2-data-json` and more specifically:

 - The `tokens` pipe to parse the input stream into JSON @:api(fs2.data.json.Token)s (see [the documentation][json-doc] for more details).
 - The @:api(fs2.data.json.jq.Compiler) class to compile a query into a pipe (see [the documentation][jq-doc] for more details).
 - The `render.pretty` pipe to render the query result into a pretty-printed JSON string (see [the documentation][render-doc] for more details).

In general the _Transforming_ step can use whatever operator fits your purpose, from `fs2` or any other `fs2`-based library. But in our case the only transformation will be performed by the query.

## Basic implementation

### Reading and writing

In this example, we will read the content from a [sample JSON file][data-json] and write the result to stdout.
To this end, we will use the operators and pipes provided by `fs2-io`.

```scala mdoc
import cats.effect.IO
import cats.effect.unsafe.implicits.global

import fs2.io.file.{Files, Path}
import fs2.io.stdout
import fs2.text.utf8

Files[IO]
  .readUtf8(Path("site/cookbooks/data/json/sample.json"))
  .through(utf8.encode[IO])
  .through(stdout)
  .compile
  .drain
  .unsafeRunSync()
```

This snippet is pure `fs2` and does not involve `fs2-data` at any point.

### Parsing and printing

The next step would be to parse and render the JSON data, using the appropriate `fs2-data` pipes. This can be achieved this way:

```scala mdoc
import fs2.data.json

Files[IO]
  .readUtf8(Path("site/cookbooks/data/json/sample.json"))
  .through(json.tokens) // parsing JSON input
  .through(json.render.pretty()) // pretty printing JSON stream
  .through(utf8.encode[IO])
  .through(stdout)
  .compile
  .drain
  .unsafeRunSync()
```

### Transforming

So far the only thing that the code does is to format the input into the output.
Looking at the input, we see that it consists in an array of objects containing several fields.
Let's say we are interested in the `name` and `language` fields.
For each element in the array we would like to emit an object with both fields, but the `name` oned should be renamed `full_name`.

To this end we can write the following query using the `jq` interpolator:

```scala mdoc
import fs2.data.json.jq.literals._

val query = jq""".[] | { "full_name": .name, "language": .language }"""
```

The query can now be compiled into a `Pipe`:

```scala mdoc
import fs2.data.json.jq.Compiler

val queryCompiler = Compiler[IO]

val queryPipe = queryCompiler.compile(query).unsafeRunSync()
```

Now this pipe can be used to transform the data within the previous pipeline

```scala mdoc
Files[IO]
  .readUtf8(Path("site/cookbooks/data/json/sample.json"))
  .through(json.tokens)
  .through(queryPipe) // the transformation using the query pipe
  .through(json.render.pretty())
  .through(utf8.encode[IO])
  .through(stdout)
  .compile
  .drain
  .unsafeRunSync()
```

And you get the result of the query execution printed to stdout.


## Running the full example

The full code can be found in the repository in the @:source(fs2.data.example.jqlike.JqLike) object.
This example uses [decline][decline] to parse the CLI options.

It compiles for all three supported platforms:

 - as a fat jar using [sbt-assembly] for JVM
 - as a native executable using [Scala Native][scala-native]
 - as a Node.js application using [Scala.js][scala-js]

@:select(platform)

@:choice(jvm)

```shell
$ sbt exampleJqJVM/assembly
$ java -jar examples/jqlike/.jvm/target/scala-2.13/jq-like.jar -q '.[] | { "full_name": .name, "language": .language }' -f site/cookbooks/data/json/sample.json
```

@:choice(native)

```shell
$ sbt exampleJqNative/nativeLink
$ examples/jqlike/.native/target/scala-2.13/jq-like-out -q '.[] | { "full_name": .name, "language": .language }' -f site/cookbooks/data/json/sample.json
```

@:choice(js)

```shell
$ sbt exampleJqJS/fastLinkJS
$ node examples/jqlike/.js/target/scala-2.13/jq-like-fastopt/main.js -q '.[] | { "full_name": .name, "language": .language }' -f site/cookbooks/data/json/sample.json
```

@:@

[decline]: https://ben.kirw.in/decline/
[data-json]: data/json/sample.json
[json-doc]: ../documentation/json/index.md#json-parsing
[jq-doc]: ../documentation/json/jq.md#using-queries
[render-doc]: ../documentation/json/index.md#json-renderers
[sbt-assembly]: https://github.com/sbt/sbt-assembly
[scala-native]: https://scala-native.org/en/latest/
[scala-js]: https://www.scala-js.org
