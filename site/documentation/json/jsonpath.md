# JSONPath

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json_2.13)

The `fs2-data-json` module provides a streaming implementation of JSONPath.

Let's use the following JSON input as an example.

```scala mdoc
import cats.syntax.all._

import fs2._
import fs2.data.json._

val input = """{
              |  "field1": 0,
              |  "field2": "test",
              |  "field3": [1, 2, 3]
              |}
              |{
              |  "field1": 2,
              |  "field3": []
              |}""".stripMargin

val stream = Stream.emit(input).through(tokens[Fallible, String])
```

## Building a JSONPath

A subset of [JSONPath][jsonpath] can be used to select a subset of a JSON token stream. There are several ways to create selectors:

 - build the selector using the constructors, which can be quite verbose and cumbersome;
 - parse a string with the JSONPath parser;
 - use the `jsonpath` interpolator.

### Parsing a string using the JSONPath parser

For instance, to select and enumerate elements that are in the `field3` array, you can create this selector. Only the tokens describing the values in `field3` will be emitted as a result.

```scala mdoc
import fs2.data.json.jsonpath._

val selector = JsonPathParser.either("$.field3[*]")
```

The JSONPath parser wraps the result in anything that has an [`MonadError` with error type `Throwable`][monad-error] to catch potential parsing errors. If you prefer not to have this wrapping, you can use the `jsonpath` interpolator.

```scala mdoc
import fs2.data.json.jsonpath.literals._

val path = jsonpath"$$.field3[*]"
```

Because `$` is a special character in interpolated strings, you need to escape it by doubling it.
The advantage of the interpolator is that potential syntax errors are checked at compilation time.

### The subset

The supported JSONPath features are:

  - `.*` selects all the object children.
  - `..*` selects all the object descendants.
  - `.id` or `["id"]` selects the object child with key `id`.
  - `..id` the recursive descent operator
  - `[idx1:idx2]` selects only elements between `idx1` (inclusive) and `idx2` (inclusive) in arrays. It fails if the value it is applied to is not an array.
  - `[idx:]` selects only elements starting from `idx1` (inclusive) until the end of the array. It fails if the value it is applied to is not an array.
  - `[:idx]` selects only elements starting from the beginning of the array up to `idx1` (inclusive). It fails if the value it is applied to is not an array.
  - `[*]` selects and enumerates elements from arrays. It fails if the value it is applied to is not an array.

## Using JSONPath

Using the path defined above, we can filter the stream of tokens, to only emit selected tokens downstream. This can be used to drastically reduce the amount of emitted data, to only the parts that are of interest for you.
The filtering pipes are located in the `fs2.data.json.jsonpath.filter` namespace.

The main operators in the namespace are:

 - `filter.first(path)` which is a `Pipe` returning the tokens of the first match only.
 - `filter.collect(path, collector)` which uses the provided `collector` to aggregate the tokens of each match, and emits all the aggregated results.
 - `filter.values[Json](path)` which builds the AST for each match for any type `Json` with a [`Builder`][json-builder] in scope.
 - `filter.deserialize[Data](path)` which deserializes matching inputs to `Data` for each match for any type `Data` with a [`Deserializer`][json-deserializer] in scope.
 - `filter.consume(path, consumer)` which sends all matches as a stream through the provided `consumer`.
 - `filter.through(path, pipe)` which sends all matches as a stream through the provided `pipe`.

@:callout(info)
Since JSONPath includes a recursive descent operator, there can be nested matches for your path.
The matches are returned in the order their first matching token is encountered in the input.
This means that for nested matches, the first stream returned is the ancestor element.
@:@


Using `filter.collect`, you can build a stream that collects each match for the provided collector and emits the aggregated result. For instance, to build the list of string representations of the matches, you can run the following code.

```scala mdoc
import fs2.data.json.literals._
import fs2.data.json.jsonpath.filter

import cats.effect._
import cats.effect.unsafe.implicits.global

val recursive = jsonpath"$$..a"

val json = json"""{
  "a": {
    "a": {
      "c": 1
    },
    "b": 2,
    "c": 3
  }
}"""

json
  .lift[IO]
  .through(filter.collect(recursive, List))
  .compile
  .toList
  .unsafeRunSync()
```

If you want to have results emitted as early as possible instead of in order, you can set the `deterministic` parameter to `false`.

```scala mdoc
json
  .lift[IO]
  .through(filter.collect(recursive, List, deterministic = false))
  .compile
  .toList
  .unsafeRunSync()
```

The `filter.consume` operator allows for consuming each match in a streaming fashion without emitting any value.
For instance, let's say you want to save each match in a file, incrementing a counter on each match. You can run the following code.

```scala mdoc
import fs2.io.file.{Files, Path}

def saveJson(counter: Ref[IO, Int], tokens: Stream[IO, Token]): Stream[IO, Nothing] =
  Stream.eval(counter.getAndUpdate(_ + 1)).flatMap { index =>
   tokens 
      .through(render.compact)
      .through(Files[IO].writeUtf8(Path(s"match-$index.json")))
  }

val program =
  for {
    counter <- Ref[IO].of(0)
    _ <- json
      .lift[IO]
      .through(filter.consume(recursive, saveJson(counter, _)))
      .compile
      .drain
  } yield ()

program.unsafeRunSync()

Files[IO].readUtf8(Path("match-0.json")).compile.string.unsafeRunSync()
Files[IO].readUtf8(Path("match-1.json")).compile.string.unsafeRunSync()
```

@:callout(warning)
The operator described below is unsafe and should be used carefully only if none of the above operators fits your purpose.
When using it, please ensure that you:

 - consume **all** inner `Stream`s
 - consume them in **parallel** (e.g. with a variant of `parEvalMap` and paralellism >1, or with a variant of `parJoin`).

Failure to do so might result in memory leaks or hanging programs.
@:@

The `filter.unsafeRaw` emits a stream of all matches.
Each match is represented as a nested stream of JSON tokens which must be consumed.

```scala mdoc

json
  .lift[IO]
  .through(filter.unsafeRaw(recursive))
  .parEvalMapUnbounded(_.compile.toList)
  .compile
  .toList
  .unsafeRunSync()
```
[monad-error]: https://typelevel.org/cats/api/cats/MonadError.html
[jsonpath]: https://goessner.net/articles/JsonPath/index.html
[json-builder]: index.md#ast-builder-and-tokenizer
[json-deserializer]: index.md#serializers-and-deserializers
