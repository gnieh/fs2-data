# fs2 data
[![Build Status](https://github.com/satabin/fs2-data/actions/workflows/ci.yml/badge.svg)](https://github.com/satabin/fs2-data/actions/workflows/ci.yml) [![Codacy Badge](https://app.codacy.com/project/badge/Grade/c3e40b51900343fc83be8179f67464cd)](https://www.codacy.com/gh/satabin/fs2-data/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=satabin/fs2-data&amp;utm_campaign=Badge_Grade) [![Gitter](https://badges.gitter.im/fs2-data/general.svg)](https://gitter.im/fs2-data/general?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

A set of streaming data parsers based on [fs2][fs2].

For more details and documentation, please visit [the website][website]

## Development

This project builds using [sbt][sbt].
* compile everything: `sbt compile`
* compile & run all tests: `sbt test`
* build the documentation: `sbt ;documentation/mdoc; makeSite`
* run benchmarks (you can provide [JMH][jmh] arguments in the end): `sbt benchmarksJVM/jmh:run`

[fs2]: https://fs2.io/
[sbt]: https://scala-sbt.org
[jmh]: https://openjdk.java.net/projects/code-tools/jmh/
[website]: https://fs2-data.gnieh.org
