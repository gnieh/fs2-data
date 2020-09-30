/*
 * Copyright 2019 Lucas Satabin
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
package fs2.data.json
package mergepatch

import cats.effect._

import ast.Tokenizer
import diffson.jsonmergepatch.JsonMergePatch

import fs2._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import diffson.Jsony

abstract class JsonMergePatchTest[Json](implicit Json: Jsony[Json], tokenizer: Tokenizer[Json])
    extends AnyFlatSpec
    with Matchers {

  def makeInt(i: Int): Json

  def makeString(s: String): Json

  def makeTrue: Json

  val valuePatch = JsonMergePatch.Value(makeInt(5))
  val nullPatch = JsonMergePatch.Value(Json.Null)
  val objectPatch = JsonMergePatch.Object(
    Map("key1" -> makeInt(3),
        "key2" -> Json.makeObject(Map("nested" -> makeString("s"), "new" -> makeTrue, "deleted" -> Json.Null))))
  val objectPatchTokens = List(
    Token.StartObject,
    Token.Key("key1"),
    Token.NumberValue("3"),
    Token.Key("key2"),
    Token.StartObject,
    Token.Key("nested"),
    Token.StringValue("s"),
    Token.Key("new"),
    Token.TrueValue,
    Token.Key("deleted"),
    Token.NullValue,
    Token.EndObject,
    Token.EndObject
  )

  "a value patch" should "replace a simple value" in {
    val patched =
      Stream
        .emits("true")
        .through(tokens[IO])
        .through(patch(valuePatch))
        .compile
        .toList
        .unsafeRunSync()
    patched shouldBe List(Token.NumberValue("5"))
  }

  it should "replace an object value" in {
    val patched =
      Stream
        .emits("""{"key": 4}""")
        .through(tokens[IO])
        .through(patch(valuePatch))
        .compile
        .toList
        .unsafeRunSync()
    patched shouldBe List(Token.NumberValue("5"))
  }

  it should "replace an array value" in {
    val patched =
      Stream
        .emits("""[1, 2, 3]""")
        .through(tokens[IO])
        .through(patch(valuePatch))
        .compile
        .toList
        .unsafeRunSync()
    patched shouldBe List(Token.NumberValue("5"))
  }

  "a null patch" should "remove a simple value" in {
    val patched =
      Stream
        .emits("true")
        .through(tokens[IO])
        .through(patch(nullPatch))
        .compile
        .toList
        .unsafeRunSync()
    patched shouldBe Nil
  }

  it should "remove an object value" in {
    val patched =
      Stream
        .emits("""{"key": 4}""")
        .through(tokens[IO])
        .through(patch(nullPatch))
        .compile
        .toList
        .unsafeRunSync()
    patched shouldBe Nil
  }

  it should "remove an array value" in {
    val patched =
      Stream
        .emits("""[1, 2, 3]""")
        .through(tokens[IO])
        .through(patch(nullPatch))
        .compile
        .toList
        .unsafeRunSync()
    patched shouldBe Nil
  }

  "an object patch" should "replace a simple value" in {
    val patched =
      Stream
        .emits("true")
        .through(tokens[IO])
        .through(patch(objectPatch))
        .compile
        .toList
        .unsafeRunSync()
    patched shouldBe objectPatchTokens
  }

  it should "replace an array value" in {
    val patched =
      Stream
        .emits("""[1, 2, 3]""")
        .through(tokens[IO])
        .through(patch(objectPatch))
        .compile
        .toList
        .unsafeRunSync()
    patched shouldBe objectPatchTokens
  }

  it should "be applied recursively to an object value" in {

    val objectTokens = List(
      Token.StartObject,
      Token.Key("untouched1"),
      Token.NumberValue("32"),
      Token.Key("key2"),
      Token.StartObject,
      Token.Key("nested"),
      Token.StringValue("s"),
      Token.Key("untouched2"),
      Token.StringValue("another string"),
      Token.Key("new"),
      Token.TrueValue,
      Token.EndObject,
      Token.Key("key1"),
      Token.NumberValue("3"),
      Token.EndObject
    )
    val patched =
      Stream
        .emits(
          """{"untouched1": 32, "key2": {"nested": "test string", "deleted": false, "untouched2": "another string"}, "key1": true}""")
        .through(tokens[IO])
        .through(patch(objectPatch))
        .compile
        .toList
        .unsafeRunSync()
    patched shouldBe objectTokens
  }

}
