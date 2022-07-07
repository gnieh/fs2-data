---
title: XPath
description: XPath support
index: 2
type: textual
module: json
---

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-xml_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-xml_2.13)

The `fs2-data-xml` module provides a streaming implementation of XPath.

This page covers the following topics:
* Contents
{:toc}

Let's use the following XML input as an example.

```scala mdoc
import cats.implicits._

import fs2._
import fs2.data.xml._

val stream = xml"""
<root>
  <a attr="value">
    <b>text</b>
    <a>
      <c><![CDATA[some other text]]></c>
    </a>
  </a>
</root>
"""
```

### XPath

A subset of [XPath][xpath] can be used to select a subset of a XML event stream. There are several ways to create selectors:
 - build the selector using the constructors, which can be quite verbose and cumbersome;
 - parse a string with the XPath parser;
 - use the `xpath` interpolator.

#### Parsing a string using the XPath parser

For instance, to select and enumerate `a` elements, you can create this selector. Only the events describing the `a` elements will be emitted as a result.

```scala mdoc
import fs2.data.xml.xpath._

val selector = XPathParser.either("//a")
```

The XPath parser wraps the result in anything that has an [`MonadError` with error type `Throwable`][monad-error] to catch potential parsing errors. If you prefer not to have this wrapping, you can use the `xpath` interpolator.

```scala mdoc
import fs2.data.xml.xpath.literals._

val path = xpath"//a"
```

The advantage of the interpolator is that potential syntax errors are checked at compilation time.

The supported XPath features are:
  - `/` the child axis
    - `/a` selects all `a` children
    - `/*` selects all children elements
  - `//` The descendent axis
    - `//a` selects all `a` descendents
    - `//*` selects all descendent elements
  - `[<attribute selection expression>]` attribute predicate selection
    - `@attr` attribute `attr` exists
    - `@attr == "value"` attribute `attr` equals `value`
    - `@attr != "value"` attribute `attr` does not equal `value`
    - `p1 && p2` element attributes match both `p1` and `p2`
    - `p1 || p2` element attributes match either `p1` or `p2`
    - `!p` element attributes do not match `p`

Operator precendence is the common one: `!` has precedence over `&&` which has precedence over `||`. This means that `!p1 && p2 || p3` is the same as `((!p1) && p2) || p3`.
You can use parenteses to associate differently, for instance `!(p1 && p2) || p3`.

#### Using XPath

Using the path defined above, we can filter the stream of events, to only emit selected tokens downstream. This can be used to drastically reduce the amount of emitted data, to only the parts that are of interest for you.
The filtering pipes are located in the `fs2.data.xml.xpath.filter` namespace.

Since XPath includes a recursive descent operator, there can be nested matches for your path.
The `filter.raw` emits a stream of all matches.
Each match is represented as a nested stream of JSON tokens which must be consumed.

```scala mdoc
import cats.Show
import cats.effect._
import cats.effect.unsafe.implicits.global

stream
  .lift[IO]
  .through(filter.raw(path))
  .parEvalMapUnbounded(_.map(Show[XmlEvent].show(_)).compile.foldMonoid)
  .compile
  .toList
  .unsafeRunSync()
```

The matching streams are returned in the order their matching element is encountered in the input.
This means that for nested matches, the first stream returned is the ancestor element.

The library offers `filter.collect` to collect each match for any collector.

```scala mdoc
stream
  .lift[IO]
  .through(filter.collect(path, collector.show))
  .compile
  .toList
  .unsafeRunSync()
```

If you want to have results emitted as early as possible instead of in order, you can set the `ordered` parameter to `false`.

```scala mdoc
stream
  .lift[IO]
  .through(filter.collect(path, collector.show, ordered = false))
  .compile
  .toList
  .unsafeRunSync()
```

[monad-error]: https://typelevel.org/cats/api/cats/MonadError.html
[xpath]: https://www.w3.org/TR/xpath/
