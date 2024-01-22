/*
 * Copyright 2024 fs2-data Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fs2
package data
package cbor

import low.{items, CborItem}
import fs2.data.json.circe._
import fs2.data.json.{Token, ast}
import fs2.io.file.{Files, Path}

import weaver._

import cats.effect._

import _root_.io.circe._
import scodec.bits._

object CborToJsonSpec extends SimpleIOSuite {

  test("Specification tests should pass") {
    Files[IO]
      .readAll(Path("cbor-json/shared/src/test/resources/appendix_a.json"))
      .through(fs2.text.utf8.decode)
      .compile
      .foldMonoid
      .map(jawn.parse(_).flatMap(_.as[List[AppendixTestCase]]))
      .rethrow
      .map(_.collect { case AppendixTestCase(cbor, _, _, Some(json), jsonAlt, _) =>
        (cbor, json, jsonAlt)
      })
      .flatMap(
        Stream
          .emits(_)
          .evalMap { case (cbor, expected, expectedAlt) =>
            Stream
              .chunk(Chunk.byteVector(cbor))
              .through(items[IO])
              .through(json.decodeItems)
              .through(ast.values)
              .compile
              .lastOrError
              .map(s => expect.same(expected, s).or(expect.same(expectedAlt, Some(s))))
          }
          .compile
          .foldMonoid)
  }

  test("Tags should be propagated to array elements") {
    Stream
      .emits(List(
        CborItem.Tag(Tags.ExpectedBase64Encoding),
        CborItem.StartArray(4),
        CborItem.ByteString(hex"6669727374"),
        CborItem.ByteString(hex"7365636f6e64"),
        CborItem.ByteString(hex"7468697264"),
        CborItem.ByteString(hex"666f75727468")
      ))
      .through(cbor.json.decodeItems[IO])
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(
            Token.StartArray,
            Token.StringValue("Zmlyc3Q="),
            Token.StringValue("c2Vjb25k"),
            Token.StringValue("dGhpcmQ="),
            Token.StringValue("Zm91cnRo"),
            Token.EndArray
          ),
          tokens
        ))
  }

  test("Byte strings are base64 URL encoded if no tag is present") {
    Stream
      .emits(List(
        CborItem.StartArray(4),
        CborItem.ByteString(hex"6669727374"),
        CborItem.ByteString(hex"7365636f6e64"),
        CborItem.ByteString(hex"7468697264"),
        CborItem.ByteString(hex"666f75727468")
      ))
      .through(cbor.json.decodeItems[IO])
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(Token.StartArray,
               Token.StringValue("Zmlyc3Q"),
               Token.StringValue("c2Vjb25k"),
               Token.StringValue("dGhpcmQ"),
               Token.StringValue("Zm91cnRo"),
               Token.EndArray),
          tokens
        ))
  }

  test("Tags should be propagated to map elements") {
    Stream
      .emits(List(
        CborItem.Tag(Tags.ExpectedBase64Encoding),
        CborItem.StartMap(4),
        CborItem.TextString("first"),
        CborItem.ByteString(hex"6669727374"),
        CborItem.TextString("second"),
        CborItem.ByteString(hex"7365636f6e64"),
        CborItem.TextString("third"),
        CborItem.ByteString(hex"7468697264"),
        CborItem.TextString("fourth"),
        CborItem.ByteString(hex"666f75727468")
      ))
      .through(cbor.json.decodeItems[IO])
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(
            Token.StartObject,
            Token.Key("first"),
            Token.StringValue("Zmlyc3Q="),
            Token.Key("second"),
            Token.StringValue("c2Vjb25k"),
            Token.Key("third"),
            Token.StringValue("dGhpcmQ="),
            Token.Key("fourth"),
            Token.StringValue("Zm91cnRo"),
            Token.EndObject
          ),
          tokens
        ))
  }

  test("Tags should be propagated to map keys") {
    Stream
      .emits(List(
        CborItem.Tag(Tags.ExpectedBase64Encoding),
        CborItem.StartMap(4),
        CborItem.ByteString(hex"6669727374"),
        CborItem.TextString("first"),
        CborItem.ByteString(hex"7365636f6e64"),
        CborItem.TextString("second"),
        CborItem.ByteString(hex"7468697264"),
        CborItem.TextString("third"),
        CborItem.ByteString(hex"666f75727468"),
        CborItem.TextString("fourth")
      ))
      .through(cbor.json.decodeItems[IO])
      .compile
      .toList
      .map(tokens =>
        expect.same(
          List(
            Token.StartObject,
            Token.Key("Zmlyc3Q="),
            Token.StringValue("first"),
            Token.Key("c2Vjb25k"),
            Token.StringValue("second"),
            Token.Key("dGhpcmQ="),
            Token.StringValue("third"),
            Token.Key("Zm91cnRo"),
            Token.StringValue("fourth"),
            Token.EndObject
          ),
          tokens
        ))
  }

}
