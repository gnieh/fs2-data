---
title: XML
description: XML parser and validator
index: 0
type: textual
module: xml
---

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-xml_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-xml_2.13)

The `fs2-data-xml` module provides tools to parse XML data in a streaming manner.

This page covers the following topics:
* Contents
{:toc}

### Basic usage

To create a stream of XML events from an input stream, use the `events` pipe in `fs2.data.xml` package.

```scala mdoc
import cats.effect._
import cats.effect.unsafe.implicits.global

import fs2._
import fs2.data.xml._

val input = """<a xmlns:ns="http://test.ns">
              |  <ns:b ns:a="attribute">text</ns:b>
              |</a>
              |<a>
              |  <b/>
              |  test entity resolution &amp; normalization
              |</a>""".stripMargin

val stream = Stream.emit(input).through(events[IO, String]())
stream.compile.toList.unsafeRunSync()
```

The pipe validates the XML structure while parsing. It reads all the XML elements in the input stream and emits events as they are available.

### Resolvers

Namespace can be resolved by using the `namespaceResolver` pipe.

```scala mdoc
val nsResolved = stream.through(namespaceResolver[IO])
nsResolved.compile.toList.unsafeRunSync()
```

Using the `referenceResolver` pipe, entity and character references can be resolved. By defaut the standard `xmlEntities` mapping is used, but it can be replaced by any mapping you see fit.

```scala mdoc
val entityResolved = stream.through(referenceResolver[IO]())
entityResolved.compile.toList.unsafeRunSync()
```

### Normalization

Once entites and namespaces are resolved, the events might be numerous and can be normalized to avoid emitting too many of them. For instance, after reference resolution, consecutive text events can be merged. This is achieved by using the `normalize` pipe.

```scala mdoc
val normalized = entityResolved.through(normalize)
normalized.compile.toList.unsafeRunSync()
```

### DOM builder and eventifier

To handle XML DOM, you can use the types and pipes available in the `fs2.data.xml.dom` package.

XML DOM can be built if you provide an implicit [`Builder[Node]`][builder-api] to the `documents` pipe. The `Builder[Node]` typeclass describes how XML DOM of type `Node` are built from an XML event stream.

```scala mdoc:compile-only
import dom._

trait SomeNodeType

implicit val builder: Builder[SomeNodeType] = ???
stream.through(documents[IO, SomeNodeType])
```

Conversely, the pipe transforming a stream of `Node`s into a stream of XML events is called `eventify` and requires an implicit [`Eventifier[Node]`][eventifier-api] in scope.

```scala mdoc:compile-only
import dom._

trait SomeNodeType

implicit val builder: Builder[SomeNodeType] = ???
implicit val eventifier: Eventifier[SomeNodeType] = ???

stream.through(documents[IO, SomeNodeType])
      .through(eventify[IO, SomeNodeType])
```

[builder-api]: /api/fs2/data/xml/dom/Builder.html
[eventifier-api]: /api/fs2/data/xml/dome/Eventifier.html
