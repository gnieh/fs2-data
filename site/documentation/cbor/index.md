# Introduction

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-cbor_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-cbor_2.13)

The `fs2-data-cbor` module provides tools to parse and transform [CBOR][cbor] data in a streaming manner.

## Low-level representation

The low-level representation lives in the `fs2.data.cbor.low` package and follows closely the structure defined in the RFC. It is a flat structure, which doesn't represent data as an AST. This representation is useful when you don't need the tree structure for your processing or when the data cannot be represented in an AST way (e.g. when a collection contains more than `Int.MaxValue` elements).

### Parsing

Parsing a CBOR binary stream into low-level representation, use the `items` pipe.

```scala mdoc
import fs2._
import fs2.data.cbor.low._

import scodec.bits._

val byteStream = Stream.chunk(Chunk.byteVector(hex"8301820203820405"))

val itemStream = byteStream.through(items[Fallible])
itemStream.compile.toList
```

### Serializing

When you already have a hand on a CBOR item stream, you can serialize it back to the binary representation (for instance to send it to the consumer) by making it go through the `toBinary` pipe.

```scala mdoc
Stream
  .emits(List(
    CborItem.StartArray(2),
    CborItem.TextString("an"),
    CborItem.TextString("array")))
  .through(toBinary[Fallible])
  .compile
  .to(ByteVector)
  .map(_.toHex)
```

The `toBinary` pipe validates the input and fail if the input steam is invalid. For instance let's say that we are missing some elements in the array.

```scala mdoc
val invalid =
  Stream
    .emits(List(
      CborItem.StartArray(3),
      CborItem.TextString("an"),
      CborItem.TextString("array")))

invalid
  .through(toBinary[Fallible])
  .compile
  .drain
```

The pipe is fine when you are sure that the input stream is valid or when throughput doesn't matter. In the case source is safe, or throughput matters more than correctness, you can use the `toNonValidatedBinary` pipe instead, which will silently generate a non valid byte stream.

```scala mdoc
val invalidBytes = invalid.through(toNonValidatedBinary)

invalidBytes
  .compile
  .to(ByteVector)
  .toHex
```

Of course, this stream will fail to be parsed.
```scala mdoc
invalidBytes.through(items[Fallible]).compile.drain
```

## High-level representation

Some transformations are easier or safe to perform on more structure data, this is a scenario in which the high-level representation can come in handy. This representation lives in the `fs2.data.cbor.high` package.

### Parsing

The main parsing pipe to get a high-level value stream from a byte stream is the `values` pipe.

```scala mdoc
import fs2.data.cbor.high._

val valueStream = byteStream.through(values[Fallible])
valueStream.compile.toList
```

If you already have a stream of low-level items, you can make it go through the `parseValues` pipe to get the value stream.

```scala mdoc
itemStream.through(parseValues[Fallible]).compile.toList
```

### Serializing

High-level value stream can be serialized to binary format by using the `toBinary` pipe.

```scala mdoc
valueStream
  .through(data.cbor.high.toBinary)
  .compile
  .to(ByteVector)
  .map(_.toHex)
```

It is possible to convert values back to low-level items, using the `toItems` pipes.

```scala mdoc
valueStream.through(toItems).compile.toList
```

You can locally control whether an array or a map will be streamed in the serialized form by setting the `indefinite` flag to `true`.

```scala mdoc
valueStream
  .map {
    // make top-level arrays streamed
    case CborValue.Array(elements, _) => CborValue.Array(elements, true)
    case value                        => value
  }
  .through(toItems)
  .compile
  .toList
```

[cbor]: https://tools.ietf.org/html/rfc7049
