# XPath

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-xml_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-xml_2.13)

The `fs2-data-xml` module provides a streaming implementation of XPath.

Let's use the following XML input as an example.

```scala mdoc
import cats.syntax.all._

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

## Building an XPath

A subset of [XPath][xpath] can be used to select a subset of a XML event stream. There are several ways to create selectors:

 - build the selector using the constructors, which can be quite verbose and cumbersome;
 - parse a string with the XPath parser;
 - use the `xpath` interpolator.

### Parsing a string using the XPath parser

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

### The subset 

The supported XPath features are:

  - `/` the child axis
    - `/a` selects all `a` children
    - `/*` selects all children elements
  - `//` The descendant axis
    - `//a` selects all `a` descendants
    - `//*` selects all descendant elements
  - `[<attribute selection expression>]` attribute predicate selection
    - `@attr` attribute `attr` exists
    - `@attr == "value"` attribute `attr` equals `value`
    - `@attr != "value"` attribute `attr` does not equal `value`
    - `p1 && p2` element attributes match both `p1` and `p2`
    - `p1 || p2` element attributes match either `p1` or `p2`
    - `!p` element attributes do not match `p`
  - `xp1|xp2` matches the XPath expression `xp1` or `xp2`

Operator precedence is the common one: `!` has precedence over `&&` which has precedence over `||`. This means that `!p1 && p2 || p3` is the same as `((!p1) && p2) || p3`.
You can use parentheses to associate differently, for instance `!(p1 && p2) || p3`.

## Using XPath

Using the path defined above, we can filter the stream of events, to only emit selected tokens downstream. This can be used to drastically reduce the amount of emitted data, to only the parts that are of interest for you.
The filtering pipes are located in the `fs2.data.xml.xpath.filter` namespace.

The main operators in the namespace are:

 - `filter.first(xpath)` which is a `Pipe` returning the events of the first match only.
 - `filter.collect(xpath, collector)` which uses the provided `collector` to aggregate the events of each match, and emits all the aggregated results.
 - `filter.dom[Node](xpath)` which builds the DOM for each match for any DOM type `Node` with a [`DocumentBuilder`][dom-builder] in scope.
 - `filter.through(xpath, pipe)` which sends all matches as a stream through the provided `pipe`.

@:callout(info)
Since XPath includes a recursive descent operator, there can be nested matches for your xpath.
The matches are returned in the order their opening matching element is encountered in the input by default.
This means that for nested matches, the first stream returned is the ancestor element.
@:@

Using `filter.collect`, you can build a stream that collects each match for the provided collector and emits the aggregated result. For instance, to build the list of string representations of the matches, you can run the following code.

```scala mdoc
import cats.effect._
import cats.effect.unsafe.implicits.global

stream
  .lift[IO]
  .through(filter.collect(path, collector.raw()))
  .compile
  .toList
  .unsafeRunSync()
```

If you want to have results emitted as early as possible instead of in order, you can set the `deterministic` parameter to `false`.

```scala mdoc
stream
  .lift[IO]
  .through(filter.collect(path, collector.raw(), deterministic = false))
  .compile
  .toList
  .unsafeRunSync()
```

The `filter.through` operator allows for handling each match in a streaming fashion.
For instance, let's say you want to save each match in a file, incrementing a counter on each match. You can run the following code.

```scala mdoc
import fs2.io.file.{Files, Path}

def saveXml(counter: Ref[IO, Int], events: Stream[IO, XmlEvent]): Stream[IO, Nothing] =
  Stream.eval(counter.getAndUpdate(_ + 1)).flatMap { index =>
    events
      .through(render.raw())
      .through(Files[IO].writeUtf8(Path(s"match-$index.xml")))
  }

val program =
  for {
    counter <- Ref[IO].of(0)
    _ <- stream
      .lift[IO]
      .through(filter.through(path, saveXml(counter, _)))
      .compile
      .drain
  } yield ()

program.unsafeRunSync()

Files[IO].readUtf8(Path("match-0.xml")).compile.string.unsafeRunSync()
Files[IO].readUtf8(Path("match-1.xml")).compile.string.unsafeRunSync()
```

@:callout(warning)
The operator described below is unsafe and should be used carefully only if none of the above operators fits your purpose.
When using it, please ensure that you:

 - consume **all** inner `Stream`s
 - consume them in **parallel** (e.g. with a variant of `parEvalMap` and paralellism >1, or with a variant of `parJoin`).

Failure to do so might result in memory leaks or hanging programs.
@:@

The `filter.unsafeRaw` operator emits a stream of all matches.
Each match is represented as a nested stream of XML events which must be consumed.

```scala mdoc
stream
  .lift[IO]
  .through(filter.unsafeRaw(path))
  .parEvalMapUnbounded(_.through(render.raw()).compile.foldMonoid)
  .compile
  .toList
  .unsafeRunSync()
```

[monad-error]: https://typelevel.org/cats/api/cats/MonadError.html
[xpath]: https://www.w3.org/TR/xpath/
[dom-builder]: index.md#dom-builder-and-eventifier
