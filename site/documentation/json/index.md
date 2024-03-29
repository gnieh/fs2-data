# Introduction

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json_2.13)

The `fs2-data-json` module provides tools to parse, query and transform JSON data in a streaming manner.

## JSON parsing

To create a stream of JSON tokens from an input stream, use the `tokens` pipe in @:api(fs2.data.json.package) package. This pipe accepts a stream of characters and returns a stream of JSON tokens. This produces a stream of structurally valid tokens forming the JSON documents.

```scala mdoc
import cats.effect._
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
stream.compile.toList
```

The pipe validates the JSON structure while parsing. It reads all the json values in the input stream and emits tokens as they are available.

## AST builder and tokenizer

To handle Json ASTs, you can use the types and pipes available in the `fs2.data.json.ast` package.

### parsing a stream to values

If you are not interested in tokens, but instead want to parse the input stream into a stream of AST values, you can use the `ast.parse` pipe, provided you have an implicit @:api(fs2.data.json.ast.Builder) in scope. The `Builder[Json]` typeclass describes how JSON ASTs of type `Json` are built from events generated by the parser.

```scala mdoc:compile-only
import ast._

trait SomeJsonType

implicit val builder: Builder[SomeJsonType] = ???

Stream.emit(input).covary[Fallible].through(parse)
```

The `ast` stream emits all top-level JSON values parsed, in our example, the two objects are emitted.

### From `Token`s to values

JSON ASTs can be built from an existing token stream, provided you have an implicit @:api(fs2.data.json.ast.Builder), using the `values` pipe.

```scala mdoc:compile-only
import ast._

trait SomeJsonType

implicit val builder: Builder[SomeJsonType] = ???

stream.through(values[Fallible, SomeJsonType])
```

**Note:** even though this snippet is equivalent in result to the one using `ast.parse` it is less efficient, and if you are only interested in the values, you should always use `ast.parse`.

### From values to `Token`s

JSON tokens can be built from an existing existing value, provided you have an implicit @:api(fs2.data.json.ast.Tokenizer), using the `tokenize` pipe.

```scala mdoc:compile-only
import ast._

trait SomeJsonType

val v: SomeJsonType = ???

implicit val tokenizer: Tokenizer[SomeJsonType] = ???

Stream.emit(v).through(tokenize[Fallible, SomeJsonType])
```

## Serializers and deserializers

To handle Json (de)serialized values, you can use the types and pipes available in the `fs2.data.json.codec` package.

Values can be automatically deserialized from a stream of JSON tokens by providing a @:api(fs2.data.json.codec.Deserializer).

```scala mdoc:compile-only
import codec._

implicit val deserializer: Deserializer[Int] = ???
stream.through(deserialize[Fallible, Int])
```

You can also serialize a stream of values by providing a @:api(fs2.data.json.codec.Serializer).

```scala mdoc:compile-only
import codec._

implicit val serializer: Serializer[String] = ???

Stream("a", "b", "c").through(serialize)
```

The `codec` package also contains various transformation pipes, acting directly on values. Please refer to the @:api(fs2.data.json.codec.package) package API documentation for more details.

For concrete examples of provided `Deserializer`s and `Serializer`s, please refer to [the JSON library binding modules documentation][json-lib-doc].

## JSON Renderers

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

There exists also a `prettyPrint()` renderer, that indents inner elements by the given indent size (in spaces) and for a given page width.

If you are interested in the String rendering as a value, you can use the `string` `Collector`:

```scala mdoc
stream.through(render.compact).compile.string

// default indentation is 2 spaces
stream.through(render.prettyPrint(width = 10)).compile.string
// if you are more into 4 spaces (or any other indentation size) you can change the indentation size
stream.through(render.prettyPrint(indent = 4, width = 10)).compile.string
```

## Generating JSON streams

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

For more pipes and options, please refer to the @:api(fs2.data.json.package$$wrap$) API documentation.

[json-lib-doc]: /documentation/json/libraries.md
[interpolator-doc]: /documentation/json/interpolators.md
