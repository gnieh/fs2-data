# Contributing to `fs2-data`

There a several ways you can contribute to `fs2-data`:
 - You found a bug? You can [open an issue][open-issue].
 - If you have an idea, found something missing, or just a question, you can also [open an issue][open-issue].
 - Code contributions are also welcome, you can [open a pull request][open-pr].
   - For coding conventions, please see below.
 - If something is broken or missing, or unclear in the documentation, you can propose a change by editing the page,
   using the pen button on the bottom right of it.

If in doubt, you can always [contact us][getting-help] and ask as many questions as you want.

## Documentation

The documentation is written in markdown and lives in the `documentation/docs` directory.
All scala code snippets must be valid, they are compiled and executed using [mdoc][mdoc].
To verify that your snippets work, run:

```shell
$ sbt documentation/mdoc
```

## Code formatting

`fs2-data` uses [scalafmt][scalafmt] to format its code. Before submitting code contribution, be sure to have proper formatting by running

```shell
$ sbt scalafmtAll
```

and check the result.

## Extend an existing data format with new features

If you wish to extend an existing data format support with new features, there are several possibilities:
 - The new feature doesn't require any extra dependency, then, it can be added to the core format module, located in `<format>/src`.
 - The new feature depends on an extra dependency, which is already present in a sub-module, then, it can be added to this sub-module, located in `<format>/<sub>/src`.
 - The new feature depends on a new extra dependency, then a new sub-module adding this dependency must be added, in `<format>/<sub>/src`.

Make sure you document your new feature in the documentation by either adding it to the existing module file, or adding a new file for a new module.

## Add support for a new data format

If you plan on contributing a new data format please bare these rules in mind:
 - `fs2-data` parsers must behave as stream parsers. This means that there must not be
   arbitrary infinite backtracking in the parser (ideally no backtracking at all).
 - The goal of the `fs2-data` parsers is that no AST is fully built in memory, but
   the sequence of emitted elements is correct _so far_. It is not a simple lexer, but
   must validate its input according to the data format grammar.

Make sure you document your new parser in the documentation by either adding a new documentation file in `documentation/docs/<format>/index.md`.

### Adding a text data format

Besides the rules mentioned above, the core module of a new data format must only depends on the `fs2-data-text` module. Parsers should be fully implemented in `fs2-data` and not rely on third party parser library. This ensures that this project controls the streaming nature of the parsers.

The parser pipe must have this signature:

```scala
def tokens[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): Pipe[F, T, Token]
```

for the `Token` type the parser defines.

Be careful when calling the `T.create` context creation function, to ensure that it is suspended properly.
A typical way of starting a parser pipe is by writing it like this:

```scala
def tokens[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): Pipe[F, T, Token] = {
  def go(context: T.Context): Pull[F, Token, Unit] =
    ???

  Stream.suspend(Stream.emit(T.create)).flatMap(go(_).stream)
}
```

For more details, you can look at existing parser implementations or ask for help on the [various channels][getting-help].

### Adding a binary data format

Besides the rules mentioned above, the core module of a new data format must only depends on the `fs2-core` module. Parsers should be fully implemented in `fs2-data` and not rely on third party parser library. This ensures that this project controls the streaming nature of the parsers.

The parser pipe must have this signature:

```scala
def tokens[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Byte, Token]
```

for the `Token` type the parser defines.

[open-issue]: https://github.com/satabin/fs2-data/issues/new/choose
[open-pr]: https://github.com/satabin/fs2-data/pull/new/main
[scalafmt]: https://scalameta.org/scalafmt/
[getting-help]: ./SUPPORT.md
[mdoc]: https://scalameta.org/mdoc/
