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
import fs2.data.json.literals._
import fs2.data.json.jsonpath._
import fs2.data.json.jsonpath.literals._

val stream = json"""{
  "field1": 0,
  "field2": "test",
  "field3": [1, 2, 3]
  }
  {
  "field1": 2,
  "field3": []
}"""

val sel = jsonpath"$$.field3[*]"
```

### Circe

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-circe_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-circe_2.13)

The `fs2-data-json-circe` module provides `Builder` and `Tokenizer` instances for the [circe][circe] `Json` type and a `Tokenizer` instance for each type `T` having an implicit `Encoder[T]` in scope.
For instance both examples from the [core module documentation][json-doc] with circe can be written that way:

```scala mdoc:nest
import fs2.data.json.circe._
import io.circe._

val asts = stream.through(ast.values[Fallible, Json])
asts.compile.toList
```

You can use `filter.values` to selects only the values matching the JSONPath and deserialize them using the builder.

```scala mdoc:nest
import fs2.data.json.circe._

import io.circe._

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
import io.circe._

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
import io.circe._
import cats.syntax.all._

val f1 = root.field("field1").compile

val transformed = stream.through(codec.transformOpt(f1, (i: Int) => (i > 0).guard[Option].as(i)))
transformed.compile.to(collector.pretty())
```

### Play! JSON

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-play_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-play_2.13)

The `fs2-data-json-play` module provides `Builder` and `Tokenizer` instances for the [Play! JSON][play-json] `JsValue` type and a `Tokenizer` instance for each type `T` having an implicit `Writes[T]` in scope.

It also provides `Deserializer` for types with a `Reads` instance and `Serializer` for the ones with a `Writes` instance.

[json-doc]: /documentation/json/
[circe]: https://circe.github.io/circe/
[play-json]: https://www.playframework.com/
