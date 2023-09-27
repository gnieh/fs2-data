---
title: JSON Libraries
index: 1
module: json
---

Bindings to popular Scala JSON libraries can be added by implementing the `Builder` and `Tokenizer` traits. `fs2-data` provides some of them out of the box.

This page covers the following libraries:
* Contents
{:toc}

Examples on this page use the following input:

```scala mdoc
import fs2.{Fallible, Stream}
import fs2.data.json._
import fs2.data.json.jsonpath._
import fs2.data.json.jsonpath.literals._

def input[F[_]] = Stream.emit("""{
  "field1": 0,
  "field2": "test",
  "field3": [1, 2, 3]
  }
  {
  "field1": 2,
  "field3": []
}""").covary[F]

val stream = input[Fallible].through(tokens)

val sel = jsonpath"$$.field3[*]"
```

### Circe

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-circe_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-circe_2.13)

The `fs2-data-json-circe` module provides `Builder` and `Tokenizer` instances for the [circe][circe] `Json` type and a `Tokenizer` instance for each type `T` having an implicit `Encoder[T]` in scope.
For instance both examples from the [core module documentation][json-doc] with circe can be written that way:

```scala mdoc:nest
import fs2.data.json.circe._

val asts = input[Fallible].through(ast.parse)
asts.map(_.spaces2).compile.toList
```

You can use `filter.values` to selects only the values matching the JSONPath and deserialize them using the builder.

```scala mdoc:nest
import fs2.data.json.circe._

import cats.effect._
import cats.syntax.all._
import cats.effect.unsafe.implicits.global

stream
  .lift[IO]
  .through(jsonpath.filter.values(sel))
  .compile
  .toList
  .unsafeRunSync()
```

The circe integration also provides `Deserializer` and `Serializer` based on the circe `Decoder`s and `Encoder`s.

Let's say we defined our data model as follows:
```scala mdoc
import io.circe.generic.JsonCodec

@JsonCodec
case class Data(field1: Int, field2: Option[String], field3: List[Int])

@JsonCodec
case class WrappedData(field1: Int, field2: Option[String], field3: List[Wrapped])

@JsonCodec
case class Wrapped(test: Int)
```
 We could write the previous wrapping transformation using these case classes.

```scala mdoc:nest
import fs2.data.json.selector._
import fs2.data.json.circe._

val values = stream.through(codec.deserialize[Fallible, Data])
values.compile.toList

def wrap(data: Data): WrappedData =
  WrappedData(data.field1, data.field2, data.field3.map(Wrapped(_)))

val sel = root.field("field3").iterate.compile

val transformed = stream.through(codec.transform(sel, wrap))
transformed.compile.to(collector.pretty())
```

Dropping values can be done similarly.

```scala mdoc:nest
import fs2.data.json.circe._
import cats.syntax.all._

val f1 = root.field("field1").compile

val transformed = stream.through(codec.transformOpt(f1, (i: Int) => (i > 0).guard[Option].as(i)))
transformed.compile.to(collector.pretty())
```

#### Migrating from `circe-fs2`

If you were using [`circe-fs2`][circe-fs2] to emit streams of `Json` values, you can easily switch to `fs2-data-json-circe`. Just replace your usages of `stringStreamParser` or `byteStreamParser` by usage of `fs2.data.json.ast.parse`.

For instance if you had this code:

```scala mdoc:nest
import io.circe.fs2._

import cats.effect._

input[SyncIO]
  .through(stringStreamParser)
  .map(_.spaces2)
  .compile
  .toList
  .unsafeRunSync()
```

You can replace it by

```scala mdoc:nest
import fs2.data.json._
import fs2.data.json.circe._

input[Fallible]
  .through(ast.parse)
  .map(_.spaces2)
  .compile
  .toList
```

If you were using `byteStreamParser`, please refer to the the [`fs2.data.text` package documentation][text] to indicate how to decode the byte stream.

### Play! JSON

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-play_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-play_2.13)

The `fs2-data-json-play` module provides `Builder` and `Tokenizer` instances for the [Play! JSON][play-json] `JsValue` type and a `Tokenizer` instance for each type `T` having an implicit `Writes[T]` in scope.

It also provides `Deserializer` for types with a `Reads` instance and `Serializer` for the ones with a `Writes` instance.

[json-doc]: /documentation/json/
[circe]: https://circe.github.io/circe/
[play-json]: https://www.playframework.com/
[circe-fs2]: https://github.com/circe/circe-fs2
[text]: /documentation/#decoding-textual-inputs
