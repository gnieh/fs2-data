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

```scala mdoc:silent
import cats.effect._

import fs2.Stream
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

val selector = ".field3.[]".parseSelector[IO].unsafeRunSync()

val filtered = stream.through(filter(selector))
```

### Circe

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-circe_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-circe_2.13)

The `fs2-data-json-circe` module provides `Builder` and `Tokenizer` instances for the [circe][circe] `Json` type.
For instance both examples from the [core module documentation][json-doc] with circe can be written that way:

```scala mdoc:nest
import fs2.data.json.circe._
import io.circe._

val asts = stream.through(values[IO, Json])
asts.compile.toList.unsafeRunSync()

val transformed = stream.through(transform[IO, Json](selector, json => Json.obj("test" -> json)))
transformed.through(values[IO, Json]).compile.toList.unsafeRunSync()
```

[json-doc]: /documentation/json/
[circe]: https://circe.github.io/circe/
