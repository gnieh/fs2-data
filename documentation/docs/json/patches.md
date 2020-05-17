---
title: JSON Patch
index: 3
module: json
---

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-diffson_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-diffson_2.13)

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
```scala mdoc
import cats.effect._

import fs2.Stream
import fs2.data.json._
import fs2.data.json.circe._
import fs2.data.json.mergepatch._

import diffson._
import diffson.circe._
import diffson.jsonmergepatch._

import io.circe._

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

// a patch that removes `field3`
val mergePatch = JsonMergePatch.Object(Map("field3" -> Json.Null))

val patched = stream.through(patch[IO, Json](mergePatch))

patched.compile.toList.unsafeRunSync()
```

[diffson]: https://github.com/gnieh/diffson
[jsonmergepatch]: https://tools.ietf.org/html/rfc7396
