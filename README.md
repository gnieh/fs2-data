# fs2-data
[![Typelevel Affiliate Project](https://img.shields.io/badge/typelevel-affiliate%20project-FFB4B5.svg)](https://typelevel.org/projects/)
[![Build Status](https://github.com/satabin/fs2-data/actions/workflows/ci.yml/badge.svg)](https://github.com/satabin/fs2-data/actions/workflows/ci.yml)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/fb0e5806644146869e7764c99b8ed3e9)](https://www.codacy.com/gh/gnieh/fs2-data/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=gnieh/fs2-data&amp;utm_campaign=Badge_Grade)
[![Discord](https://img.shields.io/discord/632277896739946517.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2)](https://discord.gg/7qNAFsYkTn)

A set of streaming data parsers based on [fs2][fs2].

For more details and documentation, please visit [the website][website]

## Development

This project builds using [sbt][sbt].
* compile everything: `sbt compile`
* compile & run all tests: `sbt test`
* build the documentation: `sbt ;documentation/mdoc; makeSite`
* run benchmarks (you can provide [JMH][jmh] arguments in the end): `sbt benchmarksJVM/jmh:run`

If you don't already have `sbt`, or if you'd like an isolated environment for development on this project, you may use the Nix shell. 
For that, you must have the [Nix package manager][nix-download] installed on your machine, and you need to enable [Nix flakes][nix-flakes] and [Nix command][nix-command]. 
With those prerequisites, from this project's root folder you just need to run `nix develop`.
If you just want to enable the experimental Nix features (command and flakes) locally and temporarily, add the `--extra-experimental-features nix-command` and the `--extra-experimental-features flakes` option/argument pairs to the `nix develop` command.

[fs2]: https://fs2.io/
[sbt]: https://scala-sbt.org
[jmh]: https://openjdk.java.net/projects/code-tools/jmh/
[website]: https://fs2-data.gnieh.org
[nix-download]: https://nixos.org/download/
[nix-command]: https://nixos.wiki/wiki/Nix_command
[nix-flakes]: https://nixos.wiki/wiki/Flakes
