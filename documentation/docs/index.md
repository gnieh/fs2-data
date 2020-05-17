---
title: Documentation
---

# `fs2-data` Documentation

`fs2-data` consists of several modules, handling a different data format, and sub-modules for each data format, adding more features (but also more dependencies). The core module for each data format has no other dependency than `fs2` and provides tools to parse and transform data in a streaming manner.

For each module, the entry point is an [fs2 `Pipe`][pipe-doc] transforming a stream of characters into some tokens or events. All parsers are streaming parsers, which means that the parsed tree is never constructed in memory by the parser, but instead, is emitted as a sequence of _tokens_ or _events_ (similar to [SAX][sax]), and that the input data is not fully held in memory either. The parsers read the input as it comes, and emit the tokens as soon as possible, discarding the input data that is not useful anymore.

As the entry points operate on stream of characters, a common pattern when using this library to read data from a file is to start by building a `Stream[F, Char]` as follows:

```scala mdoc:compile-only
import cats.effect._

import fs2._

import java.nio.file.Paths

implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

Blocker[IO].use { blocker =>
  io.file
    .readAll[IO](Paths.get("/some/path/to/a/file.data"), blocker, 1024)
    .through(text.utf8Decode)
    .flatMap(Stream.emits(_))
    // perform your parsing and transformation here
    .compile
    .drain
}
```

Available data modules are:
<% @items.find_all("/documentation/*/index.md").each do |sub| %>
 - [<%= sub[:title] %>](<%= sub.path %>) - <%= sub[:description] %>
<% end %>

[pipe-doc]: https://fs2.io/guide.html#statefully-transforming-streams
[sax]: https://en.wikipedia.org/wiki/Simple_API_for_XML
