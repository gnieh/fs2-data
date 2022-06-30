---
title: CBOR/JSON
description: CBOR/JSON interoperability library
index: 0
type: textual
module: cbor-json
---

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-cbor-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-cbor-json_2.13)

The `fs2-data-cbor-json` module provides pipes to convert streams between CBOR and JSON representations in both directions.

This page covers the following topics:
* Contents
{:toc}

### CBOR to JSON

The library implements decoding of CBOR item streams into JSON token streams.

```scala mdoc
import fs2._
import fs2.data.cbor
import fs2.data.cbor.low.CborItem

import scodec.bits._

Stream
  .emits(List(
    CborItem.PositiveInt(hex"cafebabe"),
    CborItem.StartArray(3),
    CborItem.TextString("Some string"),
    CborItem.TextString("Some other string"),
    CborItem.False
  ))
  .through(cbor.json.decodeItems[Fallible])
  .compile
  .toList
```

CBOR is more expressive than JSON and allows to express more kind of data, so not all CBOR item streams can be successfully converted, or some information loss might occur.

In particular restrictions are:
 - maps with non string (text or byte) keys are not supported
 - unknown tags are simply ignored

To convert to JSON, some tags are understood by the pipe, namely:
 - decoding of byte strings (potentially nested into arrays and maps) into base64 or base16 encoded JSON string (tags 21, 22, and 23)
 - decoding of byte strings (potentially nested into arrays and maps) into numbers (tags 2, 3, and 5)

```scala mdoc
import fs2.data.cbor.Tags

Stream
  .emits(List(
    CborItem.Tag(Tags.ExpectedBase64Encoding),
    CborItem.StartArray(2),
    CborItem.ByteString(hex"cafebabe"),
    CborItem.ByteString(hex"ff"),
    CborItem.Tag(Tags.PositiveBigNum),
    CborItem.ByteString(hex"1234567890abcdef")
  ))
  .through(cbor.json.decodeItems[Fallible])
  .compile
  .toList
```

### JSON to CBOR

Conversely, you can convert a stream of JSON tokens into a stream CBOR items. The pipe tries to encode the elements using the smallest CBOR type that can hold the value. However, some simplications are made:
 - all JSON collections (arrays and objects) generate their indefinite length CBOR counterparts
 - half-precision floats are never generated

```scala mdoc
import fs2.data.json
import fs2.data.json.Token

Stream
  .emits(List(
    Token.StartArray,
    Token.StringValue("a string"),
    Token.NumberValue("1.34"),
    Token.StartObject,
    Token.Key("a"),
    Token.TrueValue,
    Token.Key("b"),
    Token.NullValue,
    Token.EndObject,
    Token.EndArray
  ))
  .through(json.cbor.encodeItems)
  .compile
  .toList
```
