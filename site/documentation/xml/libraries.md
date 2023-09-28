# XML Libraries

Bindings to new Scala XML DOM librariy can be added by implementing the `DocumentBuilder` and `DocumentEventifier` traites. `fs2-data` provides some of them out of the box.

## `scala-xml`

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-xml-scala_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-xml-scala_2.13)

The `fs2-data-xml-scala` module provides `DocumentBuilder` and `DocumentEventifier` instances for the [scala-xml][scala-xml] `Document` type.

The `documents` Pipe emits `scala.xml.Document`s.

```scala mdoc:to-string
import fs2.{Fallible, Stream}
import fs2.data.xml._
import fs2.data.xml.dom._
import fs2.data.xml.scalaXml._

val input = """<?xml version="1.1" ?>
              |<root attr1="value1" attr2="value2">
              |  <!-- a comment -->
              |  Some text &amp; a child.
              |  <nested>With text</nested>
              |</root>""".stripMargin

val evts = Stream.emits(input)
                 .through(events[Fallible, Char]())
                 .through(documents)

evts.compile.toList
```

[scala-xml]: https://github.com/scala/scala-xml
