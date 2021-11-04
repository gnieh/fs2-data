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
import fs2.data.json.selector._

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

val sel = root.field("field3").iterate.compile

val filtered = stream.through(filter(sel))
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

val transformed = stream.through(ast.transform[Fallible, Json](sel, json => Json.obj("test" -> json)))
transformed.compile.to(collector.pretty())
```

If you want to only keep `field1` if it is greater than `1`, you can use the `transformOpt` pipe for this.

```scala mdoc:nest
import fs2.data.json.circe._
import io.circe._
import cats.implicits._

val f1 = root.field("field1").compile

val transformed = stream.through(ast.transformOpt[Fallible, Json](f1, json => json.as[Int].toOption.filter(_ > 1).as(json)))
transformed.compile.to(collector.pretty())
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
import fs2.data.json.circe._
import io.circe._

val values = stream.through(codec.deserialize[Fallible, Data])
values.compile.toList

def wrap(data: Data): WrappedData =
  WrappedData(data.field1, data.field2, data.field3.map(Wrapped(_)))

val transformed = stream.through(codec.transform(sel, wrap))
transformed.compile.to(collector.pretty())
```

Dropping values can be done similarly.

```scala mdoc:nest
import fs2.data.json.circe._
import io.circe._
import cats.implicits._

val f1 = root.field("field1").compile

val transformed = stream.through(codec.transformOpt(f1, (i: Int) => (i > 0).guard[Option].as(i)))
transformed.compile.to(collector.pretty())
```

[json-doc]: /documentation/json/
[circe]: https://circe.github.io/circe/
