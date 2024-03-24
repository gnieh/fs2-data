# Home

[![Typelevel Affiliate Project](https://img.shields.io/badge/typelevel-affiliate%20project-FFB4B5.svg)](https://typelevel.org/projects/) [![Discord](https://img.shields.io/discord/632277896739946517.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2)](https://discord.gg/7qNAFsYkTn)

`fs2-data` is a set of libraries that can be used to parse and transform data in a streaming manner. It aims at having as few dependencies as possible (usually, only [fs2][fs2]). For more details on supported format, refer to [the documentation][doc].

## Installation

Artefacts are published on maven, use your favorite build tool to bring it into your project.
Following modules are available:

  - `fs2-data-json`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json_2.13) A JSON parser and manipulation library
    - `fs2-data-json-circe`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-circe_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-circe_2.13) [circe][circe] support for parsed JSON.
    - `fs2-data-json-play`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-play_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-play_2.13) [Play! JSON][play-json] support for parsed JSON.
    - `fs2-data-json-interpolators`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-interpolators_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-interpolators_2.13) [literally][literally] support for statically checked JSON interpolators.
    - `fs2-data-json-diffson`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-diffson_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-diffson_2.13) [diffson][diffson] support for patching JSON streams.
  - `fs2-data-xml`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-xml_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-xml_2.13) An XML parser
    - `fs2-data-xml-scala`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-xml-scala_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-xml-scala_2.13) [scala-xml][scala-xml] support for XML DOM.
  - `fs2-data-csv`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-csv_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-csv_2.13) A CSV parser
    - `fs2-data-csv-generic`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-csv-generic_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-csv-generic_2.13) generic decoder for CSV files
  - `fs2-data-cbor`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-cbor_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-cbor_2.13) CBOR parser and trasformation
  - `fs2-data-cbor-json`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-cbor-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-cbor-json_2.13) CBOR/JSON interoperability library

## Adopters

Here's a (non-exhaustive) list of companies and projects that use `fs2-data`. Don't see yours? You can [add it in a PR][add-adopter]!

### Projects

 - [typelevel-toolkit](https://typelevel.org/toolkit/) - A toolkit of great libraries to start building Typelevel apps on JVM, Node.js, and Native!
 - [http4s-fs2-data](https://http4s.github.io/http4s-fs2-data/) - Provides a set of integration libraries that integrate http4s with the streaming parsers offered by fs2-data.
 - [smithy4s](https://disneystreaming.github.io/smithy4s/) - An interface definition language (IDL) provided by AWS.

### Companies
 
 - [lichess.org](https://lichess.org) - The forever free, adless and open source chess server
 - [Mobimeo](https://mobimeo.com) – Building apps and services for public transport in Germany

_Add your own_.

[doc]: /documentation/index.md
[fs2]: https://fs2.io
[circe]: https://circe.github.io/circe/
[play-json]: https://www.playframework.com/documentation/latest/ScalaJson
[diffson]: https://github.com/gnieh/diffson
[literally]: https://github.com/typelevel/literally
[scala-xml]: https://github.com/scala/scala-xml
[add-adopter]: https://github.com/gnieh/fs2-data/edit/main/site/index.md
