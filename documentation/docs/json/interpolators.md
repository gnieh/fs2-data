---
title: Interpolators
index: 2
module: json
---

Module: [![Maven Central](https://img.shields.io/maven-central/v/org.gnieh/fs2-data-json-interpolators_2.13.svg)](https://mvnrepository.com/artifact/org.gnieh/fs2-data-json-interpolators_2.13)

The `fs2-data-json-interpolators` module provides users with some useful string interpolators. The interpolators are based on [contextual][contextual] and are statically checked.

This page covers the following topics:
* Contents
{:toc}

### Selector interpolator

You can use the `selector` interpolator to parse a string.

The example above can be rewritten as:
```scala mdoc
import fs2.data.json.interpolators._

val selector = selector".field3.[]"
```

[contextual]: https://propensive.com/opensource/contextual
