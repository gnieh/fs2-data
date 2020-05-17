# fs2 data
[![Build Status](https://travis-ci.com/satabin/fs2-data.svg?branch=master)](https://travis-ci.com/satabin/fs2-data) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/920976dd1972483686e02184462f8f17)](https://www.codacy.com/app/satabin/fs2-data?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=satabin/fs2-data&amp;utm_campaign=Badge_Grade) [![Gitter](https://badges.gitter.im/fs2-data/general.svg)](https://gitter.im/fs2-data/general?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

A set of streaming data parsers based on [fs2][fs2].

For more details and documentation, please visit [the website][website]

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [JSON module usage](#json-module-usage)
  - [Stream parser](#stream-parser)
  - [Selectors](#selectors)
    - [Selector interpolators](#selector-interpolators)
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
  - [Development](#development)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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

## Development
This project builds using [mill][mill]. You can install `mill` yourself or use the provided `millw` wrapper, in this case replace `mill` with `./millw` in the following commands:
* compile everything: `mill __.compile`
* compile & run all tests: `mill __.test`
* run benchmarks (you can provide [JMH][jmh] arguments in the end): `mill '__.benchmarks[2.13.2].runJmh'`

[fs2]: https://fs2.io/
[circe]: https://circe.github.io/circe/
[contextual]: https://propensive.com/opensource/contextual
[shapeless]: https://github.com/milessabin/shapeless
[enumeratum]: https://github.com/lloydmeta/enumeratum/
[diffson]: https://github.com/gnieh/diffson
[jsonmergepatch]: https://tools.ietf.org/html/rfc7396
[mill]: https://github.com/lihaoyi/mill
[jmh]: https://openjdk.java.net/projects/code-tools/jmh/
[website]: https://fs2-data.gnieh.org
