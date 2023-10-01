# Transformations

Sometimes [JSONPath][jsonpath] and [JSON queries][jq] are not sufficient for your use case. In these cases, you can use the transformation pipes provided by the @:api(fs2.data.json.ast.transform.package) package.

## Selectors

Selectors can be used to select a subset of a JSON token stream. There are several ways to create selectors:

 - build the selector using the constructors, which can be quite verbose and cumbersome;
 - parse a string with the selector syntax;
 - use the selector DSL.

### Parsing a string using the selector syntax

For instance, to select and enumerate elements that are in the `field3` array, you can create this selector. Only the tokens describing the values in `field3` will be emitted as a result.

```scala mdoc
import fs2._
import fs2.data.json._

type ThrowableEither[T] = Either[Throwable, T]

val selector = ".field3.[]".parseSelector[ThrowableEither].toTry.get
```

The `parseSelector` method implicitly comes from the `import fs2.data.json._` and wraps the result in anything that has an [`MonadError` with error type `Throwable`][monad-error] to catch potential parsing errors. If you prefer not to have this wrapping and don't mind an extra dependency, you can have a look at [the interpolator][Parsing a string using the selector interpolator].

The filter syntax is as follows:

  - `.` selects the root values, it is basically the identity filter.
  - `.f` selects the field named `f` in objects. It fails if the value it is applied to is not a JSON object.
    - `f` must be a valid Java identifier, meaning it has to respect this regular expression: `[a-zA-Z_][a-zA-Z0-9_]*`. If you wish to select a field that doesn't respect this regular expression, you can use the syntax `.["my non-identifier field"]` described below.
    - name `f` can be immediately followed by a `!` to mark it as mandatory. Stream will fail if the end of the object the selector is applied to is reached and the field was not present in the object.
  - `.f?` is similar to `.f` but doesn't fail in case the value it is applied to is not a JSON object.
    - both `!` and `?` can be combined as `.f!?` to indicate that if the value it is applied to is a JSON object, then the field must be in it.
  - `.["f1", "f2", ..., "fn"]` selects only fields `f1` to `fn` in objects. It fails if the value it is applied to is not an object.
    - the field list can be immediately followed by a `!` to mark all fields as mandatory. Stream will fail if the end of the object the selector is applied to is reached and at least one field in the list was not present in the object.
  - `.["f1", "f2", ..., "fn"]?` is similar to `.["f1", "f2", ..., "fn"]` but doesn't fail if the value it is applied to is not an object.
    - both `!` and `?` can be combined as `.["f1", "f2", ..., "fn"]!?` to indicate that if the value it is applied to is a JSON object, then all the specified fields must be in it.
  - `.[id1, idx2, ..., idxn]` selects only elements `idx1`, ..., `idxn` in arrays. It fails if the value it is applied to is not an array.
  - `.[idx1, idx2, ..., idxn]?` is similar to `.[idx1, idx2, ..., idxn]` but doesn't fail if the value it is applied to is not an array.
  - `.[idx1:idx2]` selects only elements between `idx1` (inclusive) and `idx2` (exclusive) in arrays. It fails if the value it is applied to is not an array.
  - `.[idx1:idx2]?` is similar to `.[idx1:idx2]` but doesn't fail if the value it is applied to is not an array.
  - `.[]` selects and enumerates elements from arrays or objects. It fails if the value it is applied to is not an array or an object.
  - `.[]?` is similar as `.[]` but doesn't fail if the value it is applied to is neither an array nor an object.
  - `sel1 sel2` applies selector `sel1` to the root value, and selector `sel2` to each selected value.

### Using the selector DSL

The selector DSL is a nice way to describe selectors without using any string parsing. They also allow for programmatically building selectors.
The DSL resides within the `fs2.data.json.selector` package, and you start a selector using the `root` builder.
The selector above can be written like this with the DSL:

```scala mdoc
import fs2.data.json.selector._

val selectorFromDsl = root.field("field3").iterate.compile
```

The `.compile` in the end transforms the previous selector builder from the DSL into the final selector. Builders are safe to reuse, re-compose and compile several times.

You can express the same selectors as with the syntax described above. For instance to make the field mandatory and the iteration lenient you can do:

```scala mdoc:nest
val selectorFromDsl = root.field("field3").!.iterate.?.compile
```

The DSL is typesafe, so that you cannot write invalid selectors. Any attempt to do so results in a compilation error.

```scala mdoc:fail
// array index selection cannot be made mandatory
root.index(1).!
```

### Parsing a string using the selector interpolator

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-interpolators_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-interpolators_2.13)

The `fs2-data-json-interpolators` module provides users with some useful string interpolators. The interpolators are based on [literally][literally] and are statically checked.

You can use the `selector` interpolator to parse a literal string.

The example above can be rewritten as:
```scala mdoc
import fs2.data.json.interpolators._

val selector = selector".field3.[]"
```

## Using the selectors

All the pipes in this package are based on a selector, a @:api(fs2.data.json.ast.Builder), and a @:api(fs2.data.json.ast.Tokenizer).

If you provide an implicit @:api(fs2.data.json.ast.Tokenizer), which describes how a JSON AST is transformed into JSON tokens, you can apply transformations to the JSON stream. For instance, you can apply a function `fun` to all values in the `fields3` array by using this code:

```scala mdoc:compile-only
import ast._

trait SomeJsonType

implicit val builder: Builder[SomeJsonType] = ???
implicit val tokenizer: Tokenizer[SomeJsonType] = ???

def fun(json: SomeJsonType): SomeJsonType = ???

val stream: Stream[Fallible, Token] = ???

stream.through(transform[Fallible, SomeJsonType](selector, fun))
```
For concrete examples of provided `Builder`s and `Tokenizer`s, please refer to [the JSON library binding modules documentation][json-lib-doc].

Sometimes you would like to delete some Json values from the input stream, based o some predicate at a given path, and keep the rest untouched. In this case, you can use the `transformOpt` pipe, and return `None` for values you want to remove from the stream.

[literally]: https://github.com/typelevel/literally
[jsonpath]: jsonpath.md
[jq]: jq.md
[monad-error]: https://typelevel.org/cats/api/cats/MonadError.html
