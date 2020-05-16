# fs2 data
[![Build Status](https://travis-ci.com/satabin/fs2-data.svg?branch=master)](https://travis-ci.com/satabin/fs2-data) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/920976dd1972483686e02184462f8f17)](https://www.codacy.com/app/satabin/fs2-data?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=satabin/fs2-data&amp;utm_campaign=Badge_Grade) [![Gitter](https://badges.gitter.im/fs2-data/general.svg)](https://gitter.im/fs2-data/general?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

A set of streaming data parsers based on [fs2][fs2].

For more details and documentation, please visit [the website][website]

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [JSON module usage](#json-module-usage)
  - [Stream parser](#stream-parser)
  - [Selectors](#selectors)
    - [Selector interpolators](#selector-interpolators)
  - [AST Builder and Tokenizer](#ast-builder-and-tokenizer)
  - [Circe](#circe)
  - [Patches](#patches)
- [XML module usage](#xml-module-usage)
  - [Stream parser](#stream-parser-1)
  - [Resolvers](#resolvers)
  - [Normalization](#normalization)
- [CSV module usage](#csv-module-usage)
  - [Stream parser](#stream-parser-2)
  - [CSV Rows with headers](#csv-rows-with-headers)
  - [Decoding](#decoding)
  - [Development](#development)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## JSON module usage

### Stream parser

To create a stream of JSON tokens from an input stream, use the `tokens` pipe in `fs2.data.json` package

```scala
import cats.effect._

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

val stream = Stream.emits(input).through(tokens[IO])
println(stream.compile.toList.unsafeRunSync())
```

The pipe validates the JSON structure while parsing. It reads all the json values in the input stream and emits events as they are available.

### Selectors

Selectors can be used to select a subset of a JSON token stream.

For instance, to select and enumerate elements that are in the `field3` array, you can create this selector. Only the tokens describing the values in `field3` will be emitted as a result.

```scala
val selector = ".field3.[]".parseSelector[IO].unsafeRunSync()
val filtered = stream.through(filter(selector))
println(filtered.compile.toList.unsafeRunSync())
```

The filter syntax is as follows:
  - `.` selects the root values, it is basically the identity filter.
  - `.f` selects the field named `f` in objects. It fails if the value it is applied to is not a JSON object.
  - `.f?` is similar to `.f` but doesn't fail in case the value it is applied to is not a JSON object.
  - `.[f1, f2, ..., fn]` selects only fields `f1` to `fn` in objects. It fails if the value it is applied to is not an object.
  - `.[f1, f2, ..., fn]?` is similar to `.[f1, f2, ..., fn]` but doesn't fail if the value it is applied to is not an object.
  - `.[id1, idx2, ..., idxn]` selects only elements `idx1`, ..., `idxn` in arrays. It fails if the value it is applied to is not an array.
  - `.[idx1, idx2, ..., idxn]?` is similar to `.[idx1, idx2, ..., idxn]` but doesn't fail if the value it is applied to is not an array.
  - `.[idx1:idx2]` selects only elements between `idx1` (inclusive) and `idx2` (exclusive) in arrays. It fails if the value it is applied to is not an array.
  - `.[idx1:idx2]?` is similar to `.[idx1:idx2]` but doesn't fail if the value it is applied to is not an array.
  - `.[]` selects and enumerates elements from arrays or objects. It fails if the value it is applied to is not an array or an object.
  - `.[]?` is similar as `.[]` but doesn't fail if the value it is applied to is neither an array nor an object.
  - `sel1 sel2` applies selector `sel1` to the root value, and selector `sel2` to each selected value.

By default, selected values are emitted in the stream as they are matched, resulting in a stream with several Json values.
If this is not desired, you can wrap the elements into arrays and objects, from the root by calling `filter` with `wrap` set to `true`.

```scala
val filteredWrapped = stream.through(filter(selector, wrap = true))
println(filteredWrapped.compile.toList.unsafeRunSync())
```

If the selector selects elements in an array, then the resulting values are wrapped in an array.
On the other hand, if it selects elements in an object, then emitted values are returned wrapped in an object, associated with the last selected keys.

#### Selector interpolators

The `fs2-data-json-interpolators` module provides statically checked string interpolators. You can use the `selector` interpolator to parse a string.

The example above can be rewritten as:
```scala
import fs2.data.json.interpolators._

val selector = selector".field3.[]"
```

### AST Builder and Tokenizer

JSON ASTs can be built if you provider an implicit `Builder[Json]` to the `values` pipe. The `Builder[Json]` typeclass describes how JSON ASTs of type `Json` are built from streams.

```scala
implicit val builder: Builder[SomeJsonType] = ...
val asts = stream.through(values[F, SomeJsonType])
```

The `asts` stream emits all top-level JSON values parsed, in our example, the two objects are emitted.

If you provide an implicit `Tokenizer[Json]`, which describes how a JSON AST is transformed into JSON events, you can apply transformations to the JSON stream. For instance, you can wrap all values in the `fields3` array by using this code:

```scala
implicit tokenizer: Tokenizer[SomeJsonType] = ...
val transformed = stream.through(transform[IO, Json](selector, json => SomeJsonObject("test" -> json)))
```

### Circe

The `fs2-data-json-circe` module provides `Builder` and `Tokenizer` instances for the circe `Json` type.
For instance both examples above with circe can be written that way:

```scala
import fs2.data.json.circe._
import io.circe._

val asts = stream.through(values[F, Json])
println(asts.compile.toList.unsafeRunSync())

val transformed = stream.through(transform[IO, Json](selector, json => Json.obj("test" -> json)))
println(transformed.through(values[IO, Json]).compile.toList.unsafeRunSync())
```

### Patches

The `fs2-data-json-diffson` module provides some integration with [diffson][diffson].
It allows for patching a Json stream as it is read to emit the patched value downstream.
Patching stream can be useful in several case, for instance:
 - it can be used to filter out fields you don't need for further processing, before building an AST with these fields;
 - it can be used to make data from an input stream anonymous by removing names or identifiers;
 - it makes it possible to enrich an input stream with extra data you need for further processing;
 - many other use cases when you need to amend input data on the fly without building the entire AST in memory.

Currently only [JSON Merge Patch][jsonmergepatch] is supported.

In order for patches to be applied, you need a `Tokenizer` for some `Json` type the patch operates on (see above) and a `Jsony` from [diffson][diffson] for that same `Json` type.

Let's say you are using circe as Json AST library, you can use patches like this:
```scala
import fs2.data.json.mergepatch._

import diffson._
import diffson.circe._
import diffson.jsonmergepatch._

import io.circe._

val mergePatch: JsonMergePatch[Json] = ...

val patched = stream.through(patch(mergePatch))

println(patched.compile.toList.unsafeRunSync())
```

## XML module usage

### Stream parser

To create a stream of XML events from an input stream, use the `events` pipe in `fs2.data.xml` package

```scala
import cats.effect._

import fs2._
import fs2.data.xml._

val input = """<a xmlns:ns="http://test.ns">
              |  <ns:b ns:a="attribute">text</ns:b>
              |</a>
              |<a>
              |  <b/>
              |  test entity resolution &amp; normalization
              |</a>""".stripMargin

val stream = Stream.emits(input).through(events[IO])
println(stream.compile.toList.unsafeRunSync())
```

The pipe validates the XML structure while parsing. It reads all the XML elements in the input stream and emits events as they are available.

### Resolvers

Namespace can be resolved by using the `namespaceResolver` pipe.

```scala
val nsResolved = stream.through(namespaceResolver[IO])
println(nsResolved.compile.toList.unsafeRunSync())
```

Using the `referenceResolver` pipe, entity and character references can be resolved. By defaut the standard `xmlEntities` mapping is used, but it can be replaced by any mapping you see fit.

```scala
val entityResolved = stream.through(referenceResolver[IO]())
println(entityResolved.compile.toList.unsafeRunSync())
```

### Normalization

Once entites and namespaces are resolved, the events might be numerous and can be normalized to avoid emitting too many of them. For instance, after reference resolution, consecutive text events can be merged. This is achieved by using the `normalize` pipe.

```scala
val normalized = entityResolved.through(normalize[IO])
println(normalized.compile.toList.unsafeRunSync())
```

## Development
This project builds using [mill][mill]. You can install `mill` yourself or use the provided `millw` wrapper, in this case replace `mill` with `./millw` in the following commands:
* compile everything: `mill __.compile`
* compile & run all tests: `mill __.test`
* run benchmarks (you can provide [JMH][jmh] arguments in the end): `mill '__.benchmarks[2.13.2].runJmh'`

[fs2]: https://fs2.io/
[circe]: https://circe.github.io/circe/
[contextual]: https://propensive.com/opensource/contextual
[shapeless]: https://github.com/milessabin/shapeless
[enumeratum]: https://github.com/lloydmeta/enumeratum/
[diffson]: https://github.com/gnieh/diffson
[jsonmergepatch]: https://tools.ietf.org/html/rfc7396
[mill]: https://github.com/lihaoyi/mill
[jmh]: https://openjdk.java.net/projects/code-tools/jmh/
[website]: https://fs2-data.gnieh.org
