---
title: Documentation
---

# `fs2-data` Documentation

* Contents
{:toc}

`fs2-data` consists of several modules, handling a different data format, and sub-modules for each data format, adding more features (but also more dependencies). The core module for each data format has no other dependency than `fs2` and provides tools to parse and transform data in a streaming manner.

For each module, the entry point is an [fs2 `Pipe`][pipe-doc] transforming a stream of some input (typically `Char`, `String`, or `Byte`) into some tokens or events. All parsers are streaming parsers, which means that the parsed tree is never constructed in memory by the parser, but instead, is emitted as a sequence of _tokens_ or _events_ (similar to [SAX][sax]), and that the input data is not fully held in memory either. The parsers read the input as it comes, and emit the tokens as soon as possible, discarding the input data that is not useful anymore.

Available data modules are:
<% @items.find_all("/documentation/*/index.md").each do |sub| %>
 - [<%= sub[:title] %>](<%= sub.path %>) - <%= sub[:description] %>
<% end %>

### Parsing textual formats

The general pattern for the text parsers (JSON, XML, CSV, ...) is to have a pipe with this signature:

```scala mdoc
import fs2._
import fs2.data.text.CharLikeChunks

// the Token type depends on the format
trait Token

def tokens[F[_], T](implicit T: CharLikeChunks[F, T]): Pipe[F, T, Token] =
  s => Stream.suspend(???)
```

The input stream can be of any type `T` which can be used to read characters from. `fs2-data` provides by default implementations for types `Char` and `String`, so that you can write:

```scala mdoc
// Stream of `Char`
Stream.emits("Some input string").through(tokens[Pure, Char])

// Stream of `String`
Stream.emit("Some input string").through(tokens[Pure, String])
```

### Reading text inputs from a file

A common pattern when using this library to read data from a file is to start by build a `Stream[F, Byte]` as follows:

```scala mdoc:silent
import cats.effect._

import fs2._

import java.nio.file.Paths

implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

Blocker[IO].use { blocker =>
  io.file
    .readAll[IO](Paths.get("/some/path/to/a/file.data"), blocker, 1024)
    // perform your decoding, parsing, and transformation here
    .compile
    .drain
}
```

For textual data formats (JSON, XML, CSV, ...) this stream needs to be decoded according to the file encoding.

#### UTF-8 encoded inputs

If your file is encoded in **UTF-8**, you can use the [`fs2.text` decoding pipes][fs2-decoders] to get a stream of strings, which can then be fed to the parsers:

```scala mdoc:silent
Blocker[IO].use { blocker =>
  io.file
    .readAll[IO](Paths.get("/some/path/to/a/file.data"), blocker, 1024)
    // extra decoding step is required since UTF-8 encodes character input
    // up to 4 bytes, which might span several chunks
    .through(text.utf8Decode)
    // now that we have a stream of `String`, we can parse
    .through(tokens)
    .compile
    .drain
}
```

#### Single byte encoded (ISO-8859-1, ISO-8859-15, ASCII) inputs

If your file is encoded using a single-byte encoding, there is no built-in decoder for this in `fs2`. However, `fs2-data` provides support for common single-byte encodings, which live in the [`fs2.data.text` package][fs2-data-text-api].

```scala mdoc:silent
// for instance if your input is encoded in ISO-8859-1 aka latin1
import fs2.data.text.latin1._

Blocker[IO].use { blocker =>
  io.file
    .readAll[IO](Paths.get("/some/path/to/a/file.data"), blocker, 1024)
    // decoding is done by the now in scope `CharLikeChunks[IO, Byte]` instance
    .through(tokens)
    .compile
    .drain
}
```

[pipe-doc]: https://fs2.io/guide.html#statefully-transforming-streams
[sax]: https://en.wikipedia.org/wiki/Simple_API_for_XML
[fs2-decoders]: https://javadoc.io/doc/co.fs2/fs2-core_2.13/latest/fs2/text$.html
[fs2-data-text-api]: https://fs2-data.gnieh.org/api/fs2/data/index/text.html
