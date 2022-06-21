package fs2
package data
package cbor

import low.items
import fs2.data.json.circe._

import weaver._

import cats.syntax.all._
import cats.effect._

import _root_.io.circe._

object CborToJsonSpec extends SimpleIOSuite {

  test("Specification tests should pass") {
    fs2.io
      .readClassLoaderResource("appendix_a.json")
      .through(fs2.text.utf8.decode)
      .compile
      .foldMonoid
      .map(jawn.parse(_).flatMap(_.as[List[AppendixTestCase]]))
      .rethrow
      .map(_.zipWithIndex.collect { case (AppendixTestCase(cbor, _, _, Some(json), _), idx) => (idx, cbor, json) })
      .flatMap(
        Stream
          .emits(_)
          .evalMap { case (idx, cbor, expected) =>
            Stream
              .chunk(Chunk.byteVector(cbor))
              .through(items[IO])
              .through(fs2.data.cbor.json.tokens)
              .through(fs2.data.json.ast.values)
              .compile
              .lastOrError
              .map(s => expect(s == expected, s"failed test at index $idx"))
          }
          .compile
          .foldMonoid)
  }

}
