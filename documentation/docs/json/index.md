---
title: JSON
description: JSON parser and transformation
index: 0
module: json
---

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json_2.13)

The `fs2-data-json` module provides tools to parse, query and transform JSON data in a streaming manner.

This page covers the following topics:
* Contents
{:toc}

### Basic usage

To create a stream of JSON tokens from an input stream, use the `tokens` pipe in `fs2.data.json` package. This pipe accepts a stream of characters and returns a stream of JSON tokens. This produces a stream of structurally valid tokens forming the JSON documents.

```scala mdoc
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
stream.compile.toList.unsafeRunSync()
```

The pipe validates the JSON structure while parsing. It reads all the json values in the input stream and emits tokens as they are available.

### Selectors

Selectors can be used to select a subset of a JSON token stream.

For instance, to select and enumerate elements that are in the `field3` array, you can create this selector. Only the tokens describing the values in `field3` will be emitted as a result.

```scala mdoc
val selector = ".field3.[]".parseSelector[IO].unsafeRunSync()
val filtered = stream.through(filter(selector))
filtered.compile.toList.unsafeRunSync()
```

The `parseSelector` method implicitly comes from the `import fs2.data.json._` and wraps the result in anything that has an [`MonadError` with error type `Throwable`][monad-error] to catch potential parsing errors. If you prefer not to have this wrapping and don't mind an extra dependency, you can have a look at [the interpolator][interpolator-doc].

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

```scala mdoc
val filteredWrapped = stream.through(filter(selector, wrap = true))
filteredWrapped.compile.toList.unsafeRunSync()
```

If the selector selects elements in an array, then the resulting values are wrapped in an array.
On the other hand, if it selects elements in an object, then emitted values are returned wrapped in an object, associated with the last selected keys.

### AST builder and tokenizer

JSON ASTs can be built if you provider an implicit [`Builder[Json]`][builder-api] to the `values` pipe. The `Builder[Json]` typeclass describes how JSON ASTs of type `Json` are built from streams.

```scala
implicit val builder: Builder[SomeJsonType] = ...
val asts = stream.through(values[F, SomeJsonType])
```

The `asts` stream emits all top-level JSON values parsed, in our example, the two objects are emitted.

If you provide an implicit [`Tokenizer[Json]`][tokenizer-api], which describes how a JSON AST is transformed into JSON tokens, you can apply transformations to the JSON stream. For instance, you can wrap all values in the `fields3` array by using this code:

```scala
implicit tokenizer: Tokenizer[SomeJsonType] = ...
val transformed = stream.through(transform[IO, Json](selector, json => SomeJsonObject("test" -> json)))
```
For concrete examples of provided `Builder`s and `Tokeizer`s, please refer to [the JSON library binding modules documentation][json-lib-doc]

[json-lib-doc]: /documentation/json/libraries
[interpolator-doc]: /documentation/json/libraries
[builder-api]: /api/fs2/data/json/ast/Builder.html
[tokenizer-api]: /api/fs2/data/json/ast/Tokenizer.html
[monad-error]: https://typelevel.org/cats/api/cats/MonadError.html
