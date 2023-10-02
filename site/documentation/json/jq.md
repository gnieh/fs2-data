# JSON Queries

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json_2.13)

The `fs2-data-json` module provides a streaming implementation of a [jq][jq]-like query language.

This allows for extracting and transforming JSON data in a streaming and declarative fashion.
It can be useful when you want to extract and transform only a part of an input JSON data.

@:callout(info)
The JSON query feature is still **experimental**. It should be stable enough to be used but you can come across some bugs when using complex queries. In such a case, do not hesitate to reach out on Discord or GitHub (see link at the top of the page).
@:@

Let's use the following JSON input as an example.

```scala mdoc
import cats.effect.SyncIO
import cats.syntax.all._

import fs2._
import fs2.data.json._

val input = """{
              |  "field1": 0,
              |  "field2": "test",
              |  "field3": [1, 2, 3]
              |}""".stripMargin

val stream = Stream.emit(input).through(tokens[SyncIO, String])
```

## Building a query

There are several ways to create queries:

 - build the query using the constructors, which can be quite verbose and cumbersome;
 - parse a string with the jq parser;
 - use the `jq` interpolator.

### Parsing a string using the jq parser

For instance, to create an output array containing one element per element in `field3`, elements being objects with `field2` and the current value from `field3` we can write:

```scala mdoc
import fs2.data.json.jq._

val wrappedQuery = JqParser.either("""[ { "field2": .field2, "field3": .field3[] } ]""")
```

The jq parser wraps the result in anything that has an [`MonadError` with error type `Throwable`][monad-error] to catch potential parsing errors. If you prefer not to have this wrapping, you can use the `jq` interpolator.

```scala mdoc
import fs2.data.json.jq.literals._

val query = jq"""[ { "field2": .field2, "field3": .field3[] } ]"""
```

## The query language

The general form of a query is a two phases process:

 - A potential _filtering_ phase, which selects some elements from the input stream. If the selector is not provided, then the input value is selected as a whole.
 - A potential _construction_ phase, which builds the output based on the elements selected by the selection phase. If the constructor is not provided, then the selected elements are emitted unchanged.

The constructor can contain sub-queries applied to each selected element.

So a query is of the form:

 - `filter` to just apply a selection to input (think [JSONPath][jsonpath]).
 - `filter | constructor` to select only a subset of the input and build the result out of the selected elements.
 - `constructor` to build the result from the input top-value.

### The filters

Following filters exist:

 - `.` is the identity filter, that selects the current value.
 - `.field` or `.["field"]` is the field filter, that selects the specified field name on the current value if it is an object. If the current value is not an object, then nothing is selected.
 - `.[idx]` is the array element filter, that selects the value at the specified index in the current value if it is an array. If the current value is not an array, then nothing is selected.
 - `.[idx1:idx1]`, `.[idx:]`, `.[:idx]` is the array slice filter, that selects only elements within the provided lower bound (inclusive) and upper bound (exclusive) on the current value if it is an array. If the lower bound is not specified, then the slice is from the beginning of the array, up to the upper bound (exclusive). If the upper bound is not specified, then the slice starts from the lower bound (inclusive), up to the end of the array. Negative indices are not supported. If the current value is not an array, then nothing is selected.
 - `..` is the recursive descent filter.
 - `.[]` is the iterator filter, that selects every element on the current value if it is an object or an array. If the input is neither an object nor an array, then nothing is selected.

Filters can be sequenced by using the pipe (`|`) symbol as separator, for instance to select the field `a` and then only the third element in `a` if it is an array, the filter is `.a | .[2]`.
The pipe separator can be elided in many cases, and the leading dot that would follow it is then removed. For instance, the previous example can also be written `.a[2]`.

@:callout(warning)
The recursive descent operator must be preceded by a pipe if it is not the first operator.
@:@

### The constructors

### Values

Any JSON scalar value is a valid query constructor, it means:

 - `null`
 - `"some string"`
 - `12`, `0.1`
 - `true`, `false`

build the equivalent JSON value. The scalar values do not depend on the selected values from the input.

#### Objects

It is possible to build a JSON object, whose field values may depend on the selected elements from the filter phase, by using the following syntax: `{ "a": query1, "b": query2, ... }`.

Each object value is a full query. If a query emits several elements (e.g. is an iterator), then one object is emitted to the output per element the iterator filter selects.

@:callout(warning)
The object constructor can only contain one top-level iterator query. For instance, trying to compile the following query will **fail**:
```
{ "a": .a[], "b": .b[] }
```
@:@

#### Arrays

It is possible to build a JSON array, whose element values may depend on the selected elements from the filter phase, by using the following syntax: `[ query1, query2, ... ]`.

Each value is a full query. If a query emits several elements (e.g. is an iterator), then all resulting elements are emitted as array elements, in the order they are selected.

## Using queries

A query must first be compiled to be usable. Compiling a query can be a quite expensive computation, but a compiled query can be reused any number of time, so you usually will compile it only once.

To use a query, make your stream pass though the compiled query obtained above. A compiled query is a `Pipe[F, Token, Token]`.

```scala mdoc
val qCompiler = jq.Compiler[SyncIO]

val compiled = qCompiler.compile(query).unsafeRunSync()

stream
  .through(compiled)
  .compile
  .to(collector.pretty())
  .unsafeRunSync()
```

[jq]: https://jqlang.github.io/jq/
[jsonpath]: jsonpath.md
[monad-error]: https://typelevel.org/cats/api/cats/MonadError.html
