# fs2 data
[![Build Status](https://travis-ci.com/satabin/fs2-data.svg?branch=master)](https://travis-ci.com/satabin/fs2-data) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/920976dd1972483686e02184462f8f17)](https://www.codacy.com/app/satabin/fs2-data?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=satabin/fs2-data&amp;utm_campaign=Badge_Grade)

A set of streaming data parsers based on [fs2][fs2].

Following modules are available:
  - `fs2-data-json`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json_2.13) A JSON parser and manipulation library
  - `fs2-data-json-circe`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-circe_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-circe_2.13) [circe][circe] support for parsed JSON.
  - `fs2-data-json-diffson`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-diffson_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-diffson_2.13) [diffson][diffson] support for patching JSON streams.
  - `fs2-data-xml`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-xml_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-xml_2.13) An XML parser
  - `fs2-data-csv`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-csv_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-csv_2.13) A CSV parser
  - `fs2-data-csv-generic`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-csv-generic_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-csv-generic_2.13) generic decoder for CSV files

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [JSON module usage](#json-module-usage)
  - [Stream parser](#stream-parser)
  - [Selectors](#selectors)
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
  - `.` select the root values, it is basically the identity filter.
  - `.f` selects the field named `f` in objects. It fails if the value it is applied to is not a JSON object.
  - `.f?` is similar to `.f` but doesn't fail in case the value it is applied to is not a JSON object.
  - `.[f1, f2, ..., fn]` selects only fields `f1` to `fn` in objects. The fields are emitted wrapped in an object. It fails if the value it is applied to is not an object.
  - `.[f1, f2, ..., fn]` similar to `.[f1, f2, ..., fn]` but doesn't fail if the value it is applied to is not an object.
  - `.[id1, idx2, ..., idxn]` selects only elements `idx1`, ..., `idxn` in arrays. The values are emitted wrapped in an array. It fails if the value it is applied to is not an array.
  - `.[idx1, idx2, ..., idxn]?` similar to `.[idx1, idx2, ..., idxn]` but doesn't fail if the value it is applied to is not an array.
  - `.[idx1:idx2]` selects only elements between `idx1` (inclusive) and `idx2` (exclusive) in arrays. The values are emitted wrapped in an array. It fails if the value it is applied to is not an array.
  - `.[idx1:idx2]?` similar to `.[idx1:idx2]` but doesn't fail if the value it is applied to is not an array.
  - `.[]` selects and enumerate elements from an array or objects. The values are not wrapped in an array or object. It fails if the value it is applied to is not an array or an object.
  - `.[]?` similar as `.[]` but doesn't fail if the value it is applied to is neither an array nor an object.
  - `sel1 sel2` applies selector `sel1` to the root value, and selector `sel2` to each selected value.

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

## CSV module usage

### Stream parser

To create a stream of CSV rows from an input stream, use the `rows` pipe in `fs2.data.csv` package. The default column separator is `,` but this can be overridden by providing the `separator` parameter.

```scala
import cats.effect._

import fs2._
import fs2.data.csv._

val input = """i,s,j
              |1,test,2
              |,other,-3
              |""".stripMargin

val stream = Stream.emits(input).through(utf8EncodeC).through(rows[IO]())
println(stream.compile.toList.unsafeRunSync())
```

### CSV Rows with headers

Rows can be converted to a `CsvRow[Header]` for some `Header` type. This class provides higher-level utilities to manipulate rows.

If your CSV file doesn't have headers, you can use the `noHeaders` pipe, which creates `CsvRow[Nothing]`

```scala
val noh = stream.through(noHeaders[IO])
println(noh.compile.toList.unsafeRunSync())
```

If you want to consider the first row as a header row, you can use the `headers` pipe. For instance to have headers as `String`:

```scala
val withh = stream.through(headers[IO, String])
println(withh.map(_.toMap).compile.toList.unsafeRunSync())
```

To support your own type of `Header` you must provide an implicit `ParseableHeader[Header]`. For instance if you have a fix set of headers represented as [enumeratum][enumeratum] enum values, you can provide an instance of `ParseableHeader` as follows:

```scala
import enumeratum._

sealed trait MyHeaders extends EnumEntry
object MyHeaders extends Enum[MyHeaders] {
  case object I extends MyHeaders
  case object S extends MyHeaders
  case object J extends MyHeaders
  def values = findValues
}

implicit object ParseableMyHeaders extends ParseableHeader[MyHeaders] {
  def parse(h: String) = MyHeaders.withNameInsensitive(h)
}

val withMyHeaders = stream.through(headers[IO, MyHeaders])
println(withMyHeaders.map(_.toMap).compile.toList.unsafeRunSync())
```

If the parse method fails for a header, the entire stream fails.

### Decoding

Using the `decode` or `decodeRow` pipes, one can decode the rows into some Scala types by providing implicit instances of `RowDecoder` and `CsvRowDecoder` respectively.

The simplest way of doing it, is to use the `fs2-data-csv-generic` module, which gives automatic derivation for case classes.

For instance, to decode to a [shapeless][shapeless] `HList`

```scala
import fs2.data.csv.generic.hlist._
import shapeless._

val decodedH = stream.tail.through(decode[IO, Option[Int] :: String :: Int :: HNil]) // tail drops the header line
println(decodedH.compile.toList.unsafeRunSync())
```

Cell types (`Int`, `String`, ...) can be decoded by providing implicit instances of `CellDecoder`. Instances for primitives and common types are defined already. You can easily define your own or use generic derivation for coproducts:

```scala
import fs2.data.csv.generic.semiauto._

sealed trait State
case object On extends State
object Off extends State

implicit val stateDecoder = deriveCellDecoder[State]
// use stateDecoder to derive decoders for rows...or just test:
println(stateDecoder("On"))
println(stateDecoder("Off"))
```

The generic derivation for cell decoders also supports renaming and deriving instances for unary product types (case classes with one field):

```scala
import fs2.data.csv.generic.semiauto._

sealed trait Advanced
@CsvValue("Active") case object On extends Advanced
case class Unknown(name: String) extends Advanced

implicit val unknownDecoder = deriveCellDecoder[Unknown] // works as we have an implicit CellDecoder[String]
implicit val advancedDecoder = deriveCellDecoder[Advanced]

println(advancedDecoder("Active")) // prints Right(On)
println(advancedDecoder("Off")) // prints Right(Unknown(Off))
```

You can also decode rows to case classes automatically.

```scala
import fs2.data.csv.generic.semiauto._

case class Row(s: String, i: Option[Int], j: Int)

implicit val rowDecoder = deriveCsvRowDecoder[Row]

val rows = withh.through(decodeRow[IO, String, Row])

println(rows.compile.toList.unsafeRunSync())
```

Case class generic also supports default parameters:

```scala
case class Row(s: String, i: Int = 34, j: Int)

implicit val rowDecoder = deriveCsvRowDecoder[Row]

val rows = withh.through(decodeRow[IO, String, Row])

println(rows.compile.toList.unsafeRunSync())
```

There's also support for full auto-derivation, just `import fs2.data.csv.generic.auto._` for everything, `import fs2.data.csv.generic.auto.row._` for `RowDecoder` support only or `import fs2.data.csv.generic.auto.csvrow._` for `CsvRowDecoder` support.

### Development
This project builds using [mill][mill]. You can install `mill` yourself or use the provided `millw` wrapper, in this case replace `mill` with `./millw` in the following commands:
* compile everything: `mill __.compile`
* compile & run all tests: `mill __.test`
* run benchmarks (you can provide [JMH][jmh] arguments in the end): `mill '__.benchmarks[2.13.1].runJmh'`

[fs2]: https://fs2.io/
[circe]: https://circe.github.io/circe/
[shapeless]: https://github.com/milessabin/shapeless
[enumeratum]: https://github.com/lloydmeta/enumeratum/
[diffson]: https://github.com/gnieh/diffson
[jsonmergepatch]: https://tools.ietf.org/html/rfc7396
[mill]: https://github.com/lihaoyi/mill
[jmh]: https://openjdk.java.net/projects/code-tools/jmh/
