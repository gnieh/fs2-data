---
title: JSONPath
description: JSONPath support
index: 2
type: textual
module: json
---

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json_2.13)

The `fs2-data-json` module provides a streaming implementation of JSONPath.

This page covers the following topics:
* Contents
{:toc}

Let's use the following JSON input as an example.

```scala mdoc
import cats.implicits._

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

### JSONPath

A subset of [JSONPath][jsonpath] can be used to select a subset of a JSON token stream. There are several ways to create selectors:
 - build the selector using the constructors, which can be quite verbose and cumbersome;
 - parse a string with the JSONPath parser;
 - use the `jsonpath` interpolator.

#### Parsing a string using the JSONPath parser

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

The supported JSONPath features are:
  - `.*` selects all the object children.
  - `..*` selects all the object children.
  - `.id` or `["id"]` selects the object child with key `id`.
  - `..id` the recursive descent operator
  - `[idx1:idx2]` selects only elements between `idx1` (inclusive) and `idx2` (inclusive) in arrays. It fails if the value it is applied to is not an array.
  - `[idx:]` selects only elements starting from `idx1` (inclusive) until the end of the array. It fails if the value it is applied to is not an array.
  - `[:idx]` selects only elements starting from the beginning of the array up to `idx1` (inclusive). It fails if the value it is applied to is not an array.
  - `[*]` selects and enumerates elements from arrays. It fails if the value it is applied to is not an array.

#### Using JSONPath

Using the path defined above, we can filter the stream of tokens, to only emit selected tokens downstream. This can be used to drastically reduce the amount of emitted data, to only the parts that are of interest for you.
The filtering pipes are located in the `fs2.data.json.jsonpath.filter` namespace.

Since JSONPath includes a recursive descent operator, there can be nested matches for your path.
The `filter.raw` emits a stream of all matches.
Each match is represented as a nested stream of JSON tokens which must be consumed.

```scala mdoc
import fs2.data.json.jsonpath.filter

import cats.effect._
import cats.effect.unsafe.implicits.global

val filtered = stream.lift[IO].through(filter.raw(path)).parEvalMapUnbounded(_.compile.toList)
filtered.compile.toList.unsafeRunSync()
```

The matching streams are returned in the order their matching element is encountered in the input.
This means that for nested matches, the first stream returned is the ancestor element.

```scala mdoc
import fs2.data.json.literals._

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
  .through(filter.raw(recursive))
  .parEvalMapUnbounded(_.compile.toList)
  .compile
  .toList
  .unsafeRunSync()
```

This is actually a common use case, so the library offers `filter.collect` to have this behavior for any collector.

```scala mdoc
json
  .lift[IO]
  .through(filter.collect(recursive, List))
  .compile
  .toList
  .unsafeRunSync()
```

If you want to have results emitted as early as possible instead of in order, you can set the `ordered` parameter to `false`.

```scala mdoc
json
  .lift[IO]
  .through(filter.collect(recursive, List, ordered = false))
  .compile
  .toList
  .unsafeRunSync()
```

[monad-error]: https://typelevel.org/cats/api/cats/MonadError.html
[jsonpath]: https://goessner.net/articles/JsonPath/index.html
