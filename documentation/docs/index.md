---
title: Documentation
---

# `fs2-data` Documentation

* Contents
{:toc}

`fs2-data` consists of several modules, handling a different data format, and sub-modules for each data format, adding more features (but also more dependencies). The core module for each data format has no other dependency than `fs2` and provides tools to parse and transform data in a streaming manner.

For each module, the entry point is an [fs2 `Pipe`][pipe-doc] transforming a stream of some input (typically `Char`, `String`, or `Byte`) into some tokens or events. All parsers are streaming parsers, which means that the parsed tree is never constructed in memory by the parser, but instead, is emitted as a sequence of _tokens_ or _events_ (similar to [SAX][sax]), and that the input data is not fully held in memory either. The parsers read the input as it comes, and emit the tokens as soon as possible, discarding the input data that is not useful anymore.

Available data modules are:
<% modules_by_type.each do |type,mods| %>
 - <%= type %> format
  <% mods.each do |sub| %>
   - [<%= sub[:title] %>](<%= sub.path %>) - <%= sub[:description] %>
  <% end %>
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
import fs2.io.file.{Files, Flags, Path}

Files[IO]
  .readAll(Path("/some/path/to/a/file.data"), 1024, Flags.Read)
  // perform your decoding, parsing, and transformation here
  .compile
  .drain
```

For textual data formats (JSON, XML, CSV, ...) this stream needs to be decoded according to the file encoding.

#### Decoding textual inputs

If your file is encoded using UTF-8 or a common single-byte encoding, you can use the built-in support `fs2-data` has for these encodings, which lives in the [`fs2.data.text` package][fs2-data-text-api].

```scala mdoc:silent
// for instance if your input is encoded in ISO-8859-1 aka latin1
import fs2.data.text.latin1._
// if you have UTF-8 instead:
// import fs2.data.text.utf8._

Files[IO]
  .readAll(Path("/some/path/to/a/file.data"), 1024, Flags.Read)
  // decoding is done by the now in scope `CharLikeChunks[IO, Byte]` instance
  .through(tokens)
  .compile
  .drain
```

### Dealing with erroneous files

The default behaviour when parsing CSV files, is to terminate the stream whenever the columns of a row do not match 
with the columns of the header. If you're dealing with CSV files that could contain these kind of errors, you can make 
use of the `lenient` package. You will get back a `List` of results, where each parsed row is represented by an 
`Either[Throwable, A]`.

```scala mdoc:silent
val content =
  """name,age,description
    |John Doe,47,description 1
    |Jane Doe,50
    |Bob Smith,80,description 2
    |Alice Grey,78
    |""".stripMargin

Stream
      .emit(content)
      .covary[Fallible]
      .through(lenient.attemptDecodeGivenHeaders[TestData]())
      .compile
      .toList

// returns Either[Throwable, List[Either[Throwable, TestData]]]
```


[pipe-doc]: https://fs2.io/guide.html#statefully-transforming-streams
[sax]: https://en.wikipedia.org/wiki/Simple_API_for_XML
[fs2-decoders]: https://javadoc.io/doc/co.fs2/fs2-core_2.13/latest/fs2/text$.html
[fs2-data-text-api]: /api/fs2/data/text/index.html
