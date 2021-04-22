---
title: Home
---

# Welcome to the fs2-data website

[![Join the chat on Gitter](https://badges.gitter.im/fs2-data/general.svg)](https://gitter.im/fs2-data/general?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

`fs2-data` is a set of libraries that can be used to parse and transform data in a streaming manner. It aims at having as few dependencies as possible (usually, only [fs2][fs2]). For more details on supported format, refer to [the documentation][doc].

## Installation

Artefacts are published on maven, use your favorite build tool to bring it into your project.
Following modules are available:
  - `fs2-data-json`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json_2.13) A JSON parser and manipulation library
  - `fs2-data-json-circe`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-circe_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-circe_2.13) [circe][circe] support for parsed JSON.
  - `fs2-data-json-interpolators`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-interpolators_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-interpolators_2.13) [contextual][contextual] support for statically checked JSON interpolators.
  - `fs2-data-json-diffson`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-diffson_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-diffson_2.13) [diffson][diffson] support for patching JSON streams.
  - `fs2-data-xml`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-xml_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-xml_2.13) An XML parser
  - `fs2-data-csv`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-csv_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-csv_2.13) A CSV parser
  - `fs2-data-csv-generic`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-csv-generic_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-csv-generic_2.13) generic decoder for CSV files
  - `fs2-data-cbor`: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-cbor_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-cbor_2.13) CBOR parser and trasformation


[api]: /api/
[doc]: /documentation/
[cats-friendly-logo]: https://typelevel.org/cats/img/cats-badge-tiny.png
[fs2]: https://fs2.io
[circe]: https://circe.github.io/circe/
[diffson]: https://github.com/gnieh/diffson
[contextual]: https://propensive.com/opensource/contextual
