# fs2 data
[![Build Status](https://travis-ci.com/satabin/fs2-data.svg?branch=master)](https://travis-ci.com/satabin/fs2-data) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/920976dd1972483686e02184462f8f17)](https://www.codacy.com/app/satabin/fs2-data?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=satabin/fs2-data&amp;utm_campaign=Badge_Grade) [![Gitter](https://badges.gitter.im/fs2-data/general.svg)](https://gitter.im/fs2-data/general?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

A set of streaming data parsers based on [fs2][fs2].

For more details and documentation, please visit [the website][website]

## Development

This project builds using [mill][mill]. You can install `mill` yourself or use the provided `millw` wrapper, in this case replace `mill` with `./millw` in the following commands:
* compile everything: `mill __.compile`
* compile & run all tests: `mill __.test`
* run benchmarks (you can provide [JMH][jmh] arguments in the end): `mill '__.benchmarks[2.13.2].runJmh'`

[fs2]: https://fs2.io/
[mill]: https://github.com/lihaoyi/mill
[jmh]: https://openjdk.java.net/projects/code-tools/jmh/
[website]: https://fs2-data.gnieh.org
