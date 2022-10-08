---
title: CSV
description: CSV parser and codec
index: 0
type: textual
module: csv
---

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-csv_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-csv_2.13)

The `fs2-data-csv` module contains tools to parse and transform CSV data. These tools live in the `fs2.data.csv` package.

This page covers the following topics:
* Contents
{:toc}

### High-level and low-level APIs

`fs2-data-csv` contains two APIs which share some common concepts and are built on top of each other:
* The high-level API in `fs2.data.csv._` provides a set of `fs2.Pipe`s that are tailored towards common use cases of CSV en-/decoding. It's built by composing parts of the low-level API into a more convenient and concise interface.  
* The low-level API in `fs2.data.csv.lowlevel._` contains pipes that do only a single step in a CSV processing pipeline and are hence better building blocks for complex use cases.

Both APIs share data types and type classes that can be found in `fs2.data.csv._`.

### Quick-Start with the high-level API
For the common use case of parsing a CSV into a `case class` or serializing your data into CSV, you can use this snippet:

```scala mdoc
import cats.effect._

import fs2._
import fs2.data.csv._
import fs2.data.csv.generic.semiauto._

val input = """i,s,j
              |1,test,2
              |,other,-3
              |""".stripMargin

// Usually this would come from a file, for example using Files[IO].readAll
val textStream = Stream.emit(input).covary[Fallible]
implicit val myRowDecoder: CsvRowDecoder[MyRow, String] = deriveCsvRowDecoder
implicit val myRowEncoder: CsvRowEncoder[MyRow, String] = deriveCsvRowEncoder

// decodeUsingHeaders can take a `Char` indicating the separator to use
// for example `decodeUsingHeaders[MyRow](';') for a semi-colon separated csv
val decodedStream = textStream.through(decodeUsingHeaders[MyRow]()) 
val caseClasses = decodedStream.compile.toList

val backAsText = decodedStream.through(encodeUsingFirstHeaders(fullRows = true)).compile.string
```

This will assume the file contains headers and takes those into account, re-emitting them again on encoding. How the data is de-/encoded is determined by the type class instances of `CsvRowDecoder` and `CsvRowEncoder` which are derived semi-automatically (for details, see [the fs2-data-csv-generic module][csv-generic-doc]). The details of these type classes are described later in this document.

More high-level pipes are available for the following use cases:
* `decodeWithoutHeaders` for CSV parsing that requires no headers and none are present in the input (Note: requires `RowDecoder` instead of `CsvRowDecoder`)
* `decodeSkippingHeaders` for CSV parsing that requires no headers, but they are present in the input (Note: requires `RowDecoder` instead of `CsvRowDecoder`)
* `decodeGivenHeaders` for CSV parsing that requires headers, but they aren't present in the input
* `decodeUsingHeaders` for CSV parsing that requires headers and they're present in the input
* `encodeWithoutHeaders` for CSV encoding that works entirely without headers (Note: requires `RowEncoder` instead of `CsvRowEncoder`)
* `encodeGivenHeaders` for CSV encoding that works without headers, but they should be added to the output
* `encodeUsingFirstHeaders` for CSV encoding that works with headers. Uses the headers of the first row for the output.

### Low-level API

This section takes a closer look on the low-level concepts the high-level API is built from.

#### Parsing CSV into rows

The first one of the provided tools is the `rows` pipe, which transforms a stream of characters into a stream of parsed rows. The rows are represented by [`NonEmptyList[String]`][nel], empty lines are skipped. Values can be quoted and escaped, according to [the RFC][rfc].

```scala mdoc
val stream = textStream.through(lowlevel.rows[Fallible, String]())
stream.compile.toList
```

If your CSV data is not using the comma as separator, you can provide the character as parameter of the `rows` pipe. For instance, if your data uses semicolon as separator:

```scala mdoc
val input2 = """i;s;j
               |1;test;2""".stripMargin

val stream2 = Stream.emit(input2).through(lowlevel.rows[Fallible, String](';'))
stream2.compile.toList
```

Often, CSVs don't conform to RFC4180 and quotation marks should be treated as literal quotation marks rather than denoting a quoted value. You are able to specify quote-handling behavior to the `rows` pipe as well. For instance:

```scala mdoc
val input3 =
  """name,age,description
    |John Doe,47,no quotes
    |Jane Doe,50,"entirely quoted"
    |Bob Smith,80,"starts with" a quote
    |Alice Grey,78,contains "a quote""".stripMargin

// default quote-handling is QuoteHandling.RFCCompliant
val stream3 = Stream.emit(input3).through(lowlevel.rows[Fallible, String](',', QuoteHandling.Literal))
stream3.compile.toList
```

#### CSV rows with or without headers

Rows can be converted to a `Row` or `CsvRow[Header]` for some `Header` type. These classes provides higher-level utilities to manipulate rows.

If your CSV file doesn't have headers, you can use the `noHeaders` pipe, which creates `Row`.

```scala mdoc
val noh = stream.through(lowlevel.noHeaders)
noh.map(_.values).compile.toList
```

If you want to consider the first row as a header row, you can use the `headers` pipe. For instance to have headers as `String`:

```scala mdoc
val withh = stream.through(lowlevel.headers[Fallible, String])
withh.map(_.toMap).compile.toList
```

To support your own type of `Header` you must provide an implicit `ParseableHeader[Header]`. For instance if you have a fix set of headers represented as [enumeratum][enumeratum] enum values, you can provide an instance of `ParseableHeader` as follows:

```scala mdoc
import enumeratum._
import cats.syntax.all._
import cats.data.NonEmptyList

sealed trait MyHeaders extends EnumEntry
object MyHeaders extends Enum[MyHeaders] {
  case object I extends MyHeaders
  case object S extends MyHeaders
  case object J extends MyHeaders
  def values = findValues
}

implicit object ParseableMyHeaders extends ParseableHeader[MyHeaders] {
  def apply(names: NonEmptyList[String]): HeaderResult[MyHeaders] =
    names.traverse { name =>
      MyHeaders.withNameInsensitiveOption(name) match {
        case Some(headers) => Right(headers)
        case None          => Left(new HeaderError(s"Unknown header $name"))
      }
    }
}

val withMyHeaders = stream.through(lowlevel.headers[Fallible, MyHeaders])
withMyHeaders.map(_.toMap).compile.toList
```

If the parse method fails for a header, the entire stream fails.

#### Writing CSV
There are also pipes for encoding rows to CSV, with or without headers. Simple example without headers:

```scala mdoc
val testRows = Stream(Row(NonEmptyList.of("3", "", "test")))
testRows
  .through(lowlevel.writeWithoutHeaders)
  .through(lowlevel.toRowStrings(/* separator: Char = ',', newline: String = "\n"*/))
  .compile
  .string
```

If you want to write headers, use `writeWithHeaders` or, in case you use `CsvRow`, `encodeRowWithFirstHeaders`. For writing non-String headers, you'll need to provide an instance of `WritableHeader`, a type class analog to `ParseableHeader`.

### The type classes: Decoders and Encoders

The library also provides decoder and encoder type classes, which allow for decoding CSV rows into arbitrary data types through the `decode` and `decodeRow` pipes and encoding data to CSV via `encode` and `encodeRow`. There are several kinds:
 - `CellDecoder` is a simple value decoder, used to transform values within the CSV fields. `CellEncoder` describes the reverse operation, encoding a simple value into a String.
 - `RowDecoder` and `RowEncoder` are used to decode/encode CSV `Row`s through the `decode`/`encode` pipes, and is positional. Use it when your CSV file doesn't have headers.
 - `CsvRowDecoder` is used to decode `CsvRow`s through the `decodeRow` pipe, and has access to the headers. Use it when you want to use header names to decode the rows. Analog, `CsvRowEncoder` exists to encode values to CSV with headers.

#### `CellDecoder` & `CellEncoder`

The library provides decoders and encoders for most of the Scala common types:
   - primitive types;
   - `String`;
   - Enums;
   - `FiniteDuration`;
   - `URL`, `URI`;
   - `UUID`;
   - most common `java.time` types.

If you wish to support custom field types, you need to implement your own `CellDecoder`. To that end, you can use the convenience methods like `map` or `emap` defined on the [`CellDecoder` trait][celldecoder-scaladoc].

For instance, if you want to be able to parse an integer field into some enum, you can do:

```scala mdoc
import enumeratum.values._

sealed abstract class State(val value: Int) extends IntEnumEntry
object State extends IntEnum[State] {
  case object On extends State(1)
  case object Off extends State(0)

  def values = findValues

  implicit val decoder: CellDecoder[State] =
    CellDecoder
      .intDecoder
      .emap(withValueOpt(_)
        .liftTo[DecoderResult](new DecoderError("Unknown state")))
}
```

For `CellEncoder`, it is even easier to define your own as encoding can't fail, so basically it's just a function `A => String`. The easiest ways to roll your own are using Scala's single abstract method sugar:
```scala mdoc
case class Wrapper(content: String)
implicit val wrapperCellEncoder: CellEncoder[Wrapper] = (w: Wrapper) => w.content
```

or using `contramap` on an existing encoder:
```scala mdoc
implicit val wrapperCellEncoder2: CellEncoder[Wrapper] = CellEncoder[String].contramap(_.content)
```

#### `RowDecoder` & `RowEncoder`

`RowDecoder`s can be used to decode an entire CSV row based on field positions. For instance if you want to decode the CSV data into [shapeless][shapeless] `HList`:

```scala mdoc
import shapeless._

implicit object HListDecoder extends RowDecoder[Option[Int] :: String :: Int :: HNil] {
  def apply(row: Row): DecoderResult[Option[Int] :: String :: Int :: HNil] =
    if(row.values.size < 3)
      Left(new DecoderError("row is too short"))
    else
      for {
        i <- if(row.values.head.isEmpty) Right(None) else CellDecoder[Int].apply(row.values.head).map(Some(_))
        s <- CellDecoder[String].apply(row.values.tail.head)
        j <- CellDecoder[Int].apply(row.values.tail.tail.head)
      } yield i :: s :: j :: HNil
}

// .tail drops the header line
val hlists = noh.tail.through(lowlevel.decode[Fallible, Option[Int] :: String :: Int :: HNil])
hlists.compile.toList
```

Again, encoding is easier as it can't fail:

```scala mdoc
import shapeless._

implicit object HListEncoder extends RowEncoder[Option[Int] :: String :: Int :: HNil] {
  def apply(input: Option[Int] :: String :: Int :: HNil): Row =
    Row(NonEmptyList.of(CellEncoder[Option[Int]].apply(input.head), input.tail.head, input.tail.tail.head.toString))
}

// .tail drops the header line
val row = Stream(Option(3) :: "test" :: 42 :: HNil)
row.through(lowlevel.encode).compile.toList
```

#### `CsvRowDecoder` & `CsvRowEncoder`

If your CSV data set has headers, you can use `CsvRowDecoder`. Using the headers, one can decode the CSV data to some case class:

```scala mdoc
// note the order of fields is not the same as in the CSV data here
case class MyRow(i: Option[Int], j: Int, s: String)

implicit object MyRowDecoder extends CsvRowDecoder[MyRow, String] {
  def apply(row: CsvRow[String]): DecoderResult[MyRow] =
  for {
    i <- row.as[Int]("i").map(Some(_)).leftFlatMap(_ => Right(None))
    j <- row.as[Int]("j")
    s <- row.as[String]("s")
  } yield MyRow(i, j, s)
}
val decoded = withh.through(lowlevel.decodeRow[Fallible, String, MyRow])
decoded.compile.toList
```

Analogously, you can encode your data with a `CsvRowEncoder`. Make sure to not vary the headers based on the data itself as this can't be reflected in the CSV format.

As you can see this can be quite tedious to implement. Lucky us, the `fs2-data-csv-generic` module comes to the rescue to avoid having to write the boilerplate. Please refer to [the module documentation][csv-generic-doc] for more details.

[nel]: https://typelevel.org/cats/datatypes/nel.html
[rfc]: https://tools.ietf.org/html/rfc4180
[enumeratum]: https://github.com/lloydmeta/enumeratum/
[shapeless]: https://github.com/milessabin/shapeless
[csv-generic-doc]: /documentation/csv/generic/
[celldecoder-scaladoc]: /api/fs2/data/csv/CellDecoder.html
