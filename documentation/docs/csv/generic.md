---
title: Generic
index: 1
module: csv
---

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-csv-generic_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-csv-generic_2.13)

The `fs2-data-csv-generic` module provides automatic (Scala 2-only) and semi-automatic derivation for `RowDecoder` and `CsvRowDecoder`. 

It makes it easier to support custom row types but is based on [shapeless][shapeless], which can have a significant impact on compilation time on Scala 2. On Scala 3, it relies on mix of hand-written derivation on top of `scala.deriving.Mirror` and the more light-weight [shapeless-3][shapeless-3], so that compile times shouldn't be problematic as on Scala 2. Note that auto derivation is currently not yet supported on Scala, same goes for using default constructor arguments of `case class`es (for background see [dotty#11667][dotty#11667]).  

To demonstrate how it works, let's work again with the CSV data from the [core][csv-doc] module documentation.

```scala mdoc:reset
import fs2._
import fs2.data.csv._

val input = """i,s,j
              |1,test,2
              |,other,-3
              |""".stripMargin

val stream = Stream.emit(input).covary[Fallible]
```

This page covers the following topics:
* Contents
{:toc}

### Derivation of `CellDecoder` & `CellEncoder`

Cell types (`Int`, `String`, ...) can be decoded and encoded by providing implicit instances of `CellDecoder`/`CellEncoder`. Instances for primitives and common types are defined already. You can easily define your own or use generic derivation for coproducts:

```scala mdoc
import fs2.data.csv.generic._
import fs2.data.csv.generic.semiauto._

sealed trait State
object State {
  case object On extends State
  case object Off extends State
}

implicit val stateDecoder = deriveCellDecoder[State]
// use stateDecoder to derive decoders for rows...or just test:
stateDecoder("On")
stateDecoder("Off")

// same goes for the encoder
implicit val stateEncoder = deriveCellEncoder[State]
stateEncoder(State.On)
```

The generic derivation for cell decoders also supports renaming and deriving instances for unary product types (case classes with one field):

```scala mdoc
import fs2.data.csv.generic.semiauto._

sealed trait Advanced
object Advanced {
  @CsvValue("Active") case object On extends Advanced
  case class Unknown(name: String) extends Advanced
}

// works as we have an implicit CellDecoder[String]
implicit val unknownDecoder = deriveCellDecoder[Advanced.Unknown]
implicit val advancedDecoder = deriveCellDecoder[Advanced]

advancedDecoder("Active")
advancedDecoder("Off")

implicit val unknownEncoder = deriveCellEncoder[Advanced.Unknown]
implicit val advancedEncoder = deriveCellEncoder[Advanced]

advancedEncoder(Advanced.On)
advancedEncoder(Advanced.Unknown("Off"))
```

### Derivation of `RowDecoder` & `RowEncoder`

One can automatically derive an instance for a [shapeless][shapeless] `HList` if there are instances for all cell types. The example previously written manually now looks like:

```scala mdoc
import shapeless._
import fs2.data.csv.generic.hlist._

// .tail drops the header line
val hlists = stream.through(decodeSkippingHeaders[Option[Int] :: String :: Int :: HNil]())
hlists.compile.toList
```

### Derivation of `CsvRowDecoder`

Let's say you want to decode the CSV row to the following case class:

```scala mdoc
case class MyRow(i: Option[Int], j: Int, s: String)
```

You can get an automatically derived `CsvRowDecoder` (and a matching `CsvRowEncoder`) for every case class by importing `fs2.data.csv.generic.auto._`

```scala mdoc:nest
import fs2.data.csv.generic.auto._

val roundtrip = stream.through(decodeUsingHeaders[MyRow]())
  // and back - note that types and corresponding are all inferred
  .through(encodeUsingFirstHeaders(fullRows = true))
roundtrip.compile.string
```

Automatic derivation can be quite slow at compile time, so you might want to opt for semiautomatic derivation. In this case, you need to explicitly define the implicit instance in scope.

```scala mdoc:nest
import fs2.data.csv.generic.semiauto._

implicit val MyRowDecoder: CsvRowDecoder[MyRow, String] = deriveCsvRowDecoder[MyRow]

val decoded = stream.through(decodeUsingHeaders[MyRow]())
decoded.compile.toList
```

Both automatic and semi-automatic decoders support also default values when decoding, so instead of an `Option[Int]` for `i`, you can define this class:

```scala mdoc:nest
import fs2.data.csv.generic.auto._

case class MyRowDefault(i: Int = 42, j: Int, s: String)

val decoded = stream.through(decodeUsingHeaders[MyRowDefault]())
decoded.compile.toList
```

[csv-doc]: /documentation/csv/
[shapeless]: https://github.com/milessabin/shapeless
[shapeless-3]: https://github.com/typelevel/shapeless-3
[dotty#11667]: https://github.com/lampepfl/dotty/pull/11667
