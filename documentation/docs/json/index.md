---
title: JSON
description: JSON parser and transformation
index: 0
type: textual
module: json
---

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json_2.13)

The `fs2-data-json` module provides tools to parse, query and transform JSON data in a streaming manner.

This page covers the following topics:
* Contents
{:toc}

### JSON parsing

To create a stream of JSON tokens from an input stream, use the `tokens` pipe in `fs2.data.json` package. This pipe accepts a stream of characters and returns a stream of JSON tokens. This produces a stream of structurally valid tokens forming the JSON documents.

```scala mdoc
import cats.effect._
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
stream.compile.toList
```

The pipe validates the JSON structure while parsing. It reads all the json values in the input stream and emits tokens as they are available.

### Selectors

Selectors can be used to select a subset of a JSON token stream. There are several ways to create selectors:
 - build the selector using the constructors, which can be quite verbose and cumbersome;
 - parse a string with the selector syntax;
 - use the selector DSL.

#### Parsing a string using the selector syntax

For instance, to select and enumerate elements that are in the `field3` array, you can create this selector. Only the tokens describing the values in `field3` will be emitted as a result.

```scala mdoc

type ThrowableEither[T] = Either[Throwable, T]

val selector = ".field3.[]".parseSelector[ThrowableEither].toTry.get
```

The `parseSelector` method implicitly comes from the `import fs2.data.json._` and wraps the result in anything that has an [`MonadError` with error type `Throwable`][monad-error] to catch potential parsing errors. If you prefer not to have this wrapping and don't mind an extra dependency, you can have a look at [the interpolator][interpolator-doc].

The filter syntax is as follows:
  - `.` selects the root values, it is basically the identity filter.
  - `.f` selects the field named `f` in objects. It fails if the value it is applied to is not a JSON object.
    - `f` must be a valid Java identifier, meaning it has to respect this regular expression: `[a-zA-Z_][a-zA-Z0-9_]*`. If you wish to select a field that doesn't respect this regular expression, you can use the syntax `.["my non-identifier field"]` described below.
    - name `f` can be immediately followed by a `!` to mark it as mandatory. Stream will fail if the end of the object the selector is applied to is reached and the field was not present in the object.
  - `.f?` is similar to `.f` but doesn't fail in case the value it is applied to is not a JSON object.
    - both `!` and `?` can be combined as `.f!?` to indicate that if the value it is applied to is a JSON object, then the field must be in it.
  - `.["f1", "f2", ..., "fn"]` selects only fields `f1` to `fn` in objects. It fails if the value it is applied to is not an object.
    - the field list can be immediately followed by a `!` to mark all fields as mandatory. Stream will fail if the end of the object the selector is applied to is reached and at least one field in the list was not present in the object.
  - `.["f1", "f2", ..., "fn"]?` is similar to `.["f1", "f2", ..., "fn"]` but doesn't fail if the value it is applied to is not an object.
    - both `!` and `?` can be combined as `.["f1", "f2", ..., "fn"]!?` to indicate that if the value it is applied to is a JSON object, then all the specified fields must be in it.
  - `.[id1, idx2, ..., idxn]` selects only elements `idx1`, ..., `idxn` in arrays. It fails if the value it is applied to is not an array.
  - `.[idx1, idx2, ..., idxn]?` is similar to `.[idx1, idx2, ..., idxn]` but doesn't fail if the value it is applied to is not an array.
  - `.[idx1:idx2]` selects only elements between `idx1` (inclusive) and `idx2` (exclusive) in arrays. It fails if the value it is applied to is not an array.
  - `.[idx1:idx2]?` is similar to `.[idx1:idx2]` but doesn't fail if the value it is applied to is not an array.
  - `.[]` selects and enumerates elements from arrays or objects. It fails if the value it is applied to is not an array or an object.
  - `.[]?` is similar as `.[]` but doesn't fail if the value it is applied to is neither an array nor an object.
  - `sel1 sel2` applies selector `sel1` to the root value, and selector `sel2` to each selected value.

#### Using the selector DSL

The selector DSL is a nice way to describe selectors without using any string parsing. They also allow for programmatically building selectors.
The DSL resides within the `fs2.data.json.selector` package, and you start a selector using the `root` builder.
The selector above can be written like this with the DSL:

```scala mdoc
import fs2.data.json.selector._

val selectorFromDsl = root.field("field3").iterate.compile
```

The `.compile` in the end transforms the previous selector builder from the DSL into the final selector. Builders are safe to reuse, re-compose and compile several times.

You can express the same selectors as with the syntax described above. For instance to make the field mandatory and the iteration lenient you can do:

```scala mdoc:nest
val selectorFromDsl = root.field("field3").!.iterate.?.compile
```

The DSL is typesafe, so that you cannot write invalid selectors. Any attempt to do so results in a compilation error.

```scala mdoc:fail
// array index selection cannot be made mandatory
root.index(1).!
```

```scala mdoc:fail
// you cannot use the same flag twice
root.index(1).?.?
```

### AST builder and tokenizer

To handle Json ASTs, you can use the types and pipes available in the `fs2.data.json.ast` package.

JSON ASTs can be built if you provide an implicit [`Builder[Json]`][builder-api] to the `values` pipe. The `Builder[Json]` typeclass describes how JSON ASTs of type `Json` are built from streams.

```scala mdoc:compile-only
import ast._

trait SomeJsonType

implicit val builder: Builder[SomeJsonType] = ???
stream.through(values[Fallible, SomeJsonType])
```

The `asts` stream emits all top-level JSON values parsed, in our example, the two objects are emitted.

If you provide an implicit [`Tokenizer[Json]`][tokenizer-api], which describes how a JSON AST is transformed into JSON tokens, you can apply transformations to the JSON stream. For instance, you can apply a function `fun` to all values in the `fields3` array by using this code:

```scala mdoc:compile-only
import ast._

trait SomeJsonType

implicit val builder: Builder[SomeJsonType] = ???
implicit val tokenizer: Tokenizer[SomeJsonType] = ???

def fun(json: SomeJsonType): SomeJsonType = ???

stream.through(transform[Fallible, SomeJsonType](selector, fun))
```
For concrete examples of provided `Builder`s and `Tokenizer`s, please refer to [the JSON library binding modules documentation][json-lib-doc].

Sometimes you would like to delete some Json values from the input stream, based o some predicate at a given path, and keep the rest untouched. In this case, you can use the `transformOpt` pipe, and return `None` for values you want to remove from the stream.

### Serializers and deserializers

To handle Json (de)serialized values, you can use the types and pipes available in the `fs2.data.json.codec` package.

Values can be automatically deserialized from a stream of JSON tokens by providing a [`Deserializer[T]`][deserializer-api].

```scala mdoc:compile-only
import codec._

implicit val deserializer: Deserializer[Int] = ???
stream.through(deserialize[Fallible, Int])
```

You can also serialize a stream of values by providing a [`Serializer[T]`][serializer-api].

```scala mdoc:compile-only
import codec._

implicit val serializer: Serializer[String] = ???

Stream("a", "b", "c").through(serialize)
```

The `codec` package also contains various transformation pipes, acting directly on values. Please refer to the [package API documentation][codec-api] for more details.

For concrete examples of provided `Deserializer`s and `Serializer`s, please refer to [the JSON library binding modules documentation][json-lib-doc].

### JSON Renderers

Once you got a JSON token stream, selected and transformed what you needed in it, you can then write the resulting token stream to some storage. This can be achieved using renderers.

For instance, let's say you want to write the resulting JSON stream to a file in compact form (i.e. with no space or new lines), you can do:

```scala mdoc:compile-only
import fs2.io.file.{Files, Flags, Path}

stream
  .through(render.compact)
  .through(text.utf8.encode)
  .lift[IO]
  .through(Files[IO].writeAll(Path("/some/path/to/file.json"), Flags.Write))
  .compile
  .drain
```

There exists also a `pretty()` renderer, that indents inner elements by the given indent string.

If you are interested in the String rendering as a value, the library also provides [`Collector`s][collector-doc]:

```scala mdoc
stream.compile.to(collector.compact)

// default indentation is 2 spaces
stream.compile.to(collector.pretty())
// if you are more into tabs (or any other indentation size) you can change the indentation string
stream.compile.to(collector.pretty("\t"))
```

### Generating JSON streams

Another use case of the library can be to generate a JSON token stream. This comes in handy if you are developing a web service that returns some big JSON in chunks.

To this end you can use the pipes in `wrap` which allow you to wrap a stream into an object structure.

For instance imagine you have a store of events which can return a stream of events, and you have a way to serialize the events into JSON.
```scala mdoc
sealed trait Event
case class CreateCounter(name: String, initialValue: Int) extends Event
case class RemoveCounter(name: String) extends Event
case class IncreaseCounter(name: String) extends Event

object Event {
  import _root_.io.circe.Encoder
  import _root_.io.circe.generic.extras.Configuration
  import _root_.io.circe.generic.extras.semiauto._
  implicit val configuration = Configuration.default.withDiscriminator("type")

  implicit val encoder: Encoder[Event] = deriveConfiguredEncoder
}

val events = Stream.emits(
  List[Event](
    CreateCounter("counter1", 0),
    IncreaseCounter("counter1"),
    CreateCounter("counter2", 0),
    RemoveCounter("counter2")
  )
)
```

You can generate a stream of JSON token wrapped in an object at a key named `events` like this:

```scala mdoc
import fs2.data.json.circe._

val wrappedTokens = events.through(ast.tokenize).through(wrap.asArrayInObject(at = "events"))
```

You can use the renderers described above to generate the rendered chunks to send to the client.

```scala mdoc
wrappedTokens.through(render.compact).compile.toList
```

You can also add other fields to the the generated object stream. For instance, let's assume we can know how big the stream will be in advance from our event store, we can send this piece of data in the first chunks, so that the client can react accordingly.

```scala mdoc
import _root_.io.circe.Json

events
  .through(ast.tokenize)
  .through(wrap.asArrayInObject(at = "events", in = Map("size" -> Json.fromInt(4))))
  .through(render.compact)
  .compile
  .toList
```

For more pipes and options, please refer to the [API documentation][wrap-api].

[json-lib-doc]: /documentation/json/libraries
[interpolator-doc]: /documentation/json/libraries
[builder-api]: /api/fs2/data/json/ast/Builder.html
[tokenizer-api]: /api/fs2/data/json/ast/Tokenizer.html
[deserializer-api]: /api/fs2/data/json/codec/Deserializer.html
[serializer-api]: /api/fs2/data/json/codec/Serializer.html
[codec-api]: /api/fs2/data/json/codec/
[wrap-api]: /api/fs2/data/json/package$$wrap$.html
[monad-error]: https://typelevel.org/cats/api/cats/MonadError.html
[collector-doc]: https://oss.sonatype.org/service/local/repositories/releases/archive/co/fs2/fs2-core_2.13/2.3.0/fs2-core_2.13-2.3.0-javadoc.jar/!/fs2/Collector.html
