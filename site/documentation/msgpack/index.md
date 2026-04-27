# Introduction

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-msgpack_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-msgpack_2.13)

The `fs2-data-msgpack` module provides tools that allow for serialization and deserialziation of [MessagePack](https://msgpack.org/) data.

The module is split into two namespaces:
* `fs2.data.msgpack.low` - Works on a flat model (see @:api(msgpack.low.MsgpackItem$)) which mimics the raw MessagePack format. This model is used as a middle-point for the high-level API.
* `fs2.data.msgpack.high` - Works on Scala values directly. You can create custom serializers and deserializers for your classes.

# High-level API
This is probably the API you want to use (unless you need to work on a flat stream instead of an AST).

## Serialization

To serialize a class, you first have to provide a `MsgpackSerializer[A]` instance for it.

`MsgpackSerializer[A]` describes a process of turning a value of type `A` into an array of `MsgpackItem`s.

```scala mdoc:silent
import fs2.*
import fs2.data.msgpack.high.*
import java.time.Instant

case class User(name: String, age: Int, created: Instant, visits: List[Long])

implicit val userSerializer: MsgpackSerializer[User] = {
  case User(name, age, created, visits) =>
    for {
      name    <- MsgpackSerializer[String](name)
      age     <- MsgpackSerializer[Int](age)
      created <- MsgpackSerializer[Instant](created)
      visits  <- MsgpackSerializer[List[Long]](visits)
    } yield name ++ age ++ created ++ visits
  }
```

The `serialize: Pipe[F, A, Byte]` method can be used to apply a serializer of a specific type onto a stream.

```scala mdoc
val inputStream = Stream[Fallible, User](
  User("foo", 30, Instant.parse("2025-02-03T10:00:00.00Z"), List.empty),
  User("bar", 31, Instant.parse("2026-01-17T10:00:00.00Z"), List(1, 3, 7))
)

val byteStream = inputStream.through(data.msgpack.high.serialize[Fallible, User])
```

You can also provide an instance of `MsgpackSerializer` to `serialize` as an argument explicitly.
Serializer instances for common types are imported from the `fs2.data.msgpack.high` package.

## Deserialization

Similarly to serialization, you have to provide a `MsgpackDeserializer[A]` instance for your type before you can parse it.

`MsgpackDeserializer[A]` describes a process of creating a value of type `A` from a chunk of `MsgpackItems`s.

```scala mdoc:silent
implicit val userDeserializer: MsgpackDeserializer[User] = 
  for {
    name    <- MsgpackDeserializer[String]
    age     <- MsgpackDeserializer[Int]
    created <- MsgpackDeserializer[Instant]
    visits <- MsgpackDeserializer[List[Long]]
  } yield User(name, age, created, visits)
```

Deserialization can be done with `deserialize: Pipe[F, Byte, A]`.

```scala mdoc
byteStream
  .through(data.msgpack.high.deserialize[Fallible, User])
  .compile.toList
```

## AST
More dynamic behavior can be achieved via the `msgpack.high.ast` API.
The AST module exports a serializer and a deserializer for @:api(msgpack.high.ast.MsgpackValue$).
In general, the AST API allows for more flexible behavior (e.g. heterogeneous lists) but also has worse performance (both in terms of speed and memory usage).

```scala mdoc
val valueStream = byteStream.through(data.msgpack.high.ast.valuesFromBytes)

val byteStream2 = valueStream.through(data.msgpack.high.ast.valuesToBytes)

valueStream.compile.toList

byteStream2.compile.toList == byteStream.compile.toList
```

# Low-level API
In case you want to operate on a flat MessagePack stream directly, you can use the low-level API.
Methods exposed in `fs2.data.msgpack.low` translate the binary data to @:api(msgpack.low.MsgpackItem$) and vice versa.

## Serialization
The main serialization pipe is called `toBinary: Pipe[F, MsgpackItem, Byte]`.

```scala mdoc:reset
import fs2.*
import fs2.data.msgpack.low.*
import scodec.bits.*

val inputStream = Stream[Fallible, MsgpackItem](
  MsgpackItem.Array(2),
  MsgpackItem.UnsignedInt(hex"ab"),
  MsgpackItem.UnsignedInt(hex"abcdef01")
)

val binaryStream = inputStream.through(data.msgpack.low.toBinary)
```

This method performs stream validation and will not emit incorrect data.
In case you are sure that the item stream is valid, you can use the `nonValidatedBinary` pipe.

```scala mdoc
inputStream.through(data.msgpack.low.toNonValidatedBinary)
```

You can also apply the `validate: Pipe[F, MsgpackItem, MsgpackItem]` pipe directly onto the stream.
This method will raise inside the effect in a case of a malformed stream.

```scala mdoc
Stream(MsgpackItem.Array(999))
  .through(data.msgpack.low.validate[Fallible])
  .compile.toList
```


## Deserialization
You can convert the stream of `Byte`s into a stream of @:api(msgpack.low.MsgpackItem$)s by using the `fromBinary: Pipe[F, Byte, MsgpackItem]` pipe.

Excluding extension types, the process of deserialization is the inverse of serialization.
In other words, `stream.through(serialize).through(deserialize)` should always be equal to `stream`.

```scala mdoc
val itemStream = binaryStream.through(data.msgpack.low.fromBinary[Fallible])

itemStream.compile.toList == inputStream.compile.toList
```

# Converting between low-level and high-level representations
As mentioned before, the flat item stream present in low-level API serves as a middle point between the binary data and language-level values.
To translate between Scala objects and `MsgpackItem` instances you can use the following functions:
* `high.fromItems: Pipe[F, MsgpackItem, A]`
* `high.toItems: Pipe[F, A, MsgpackItem]`
* `high.ast.valuesFromItems: Pipe[F, MsgpackItem, MsgpackValue]`
* `high.ast.valuesToItems: Pipe[F, MsgpackValue, MsgpackItem]`
