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

import circe._

import diffson.jsonmergepatch.JsonMergePatch
import diffson.circe._

import io.circe._

import fs2._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JsonMergePatchTest extends AnyFlatSpec with Matchers {

  val valuePatch = JsonMergePatch.Value(Json.fromInt(5))
  val nullPatch = JsonMergePatch.Value(Json.Null)
  val objectPatch = JsonMergePatch.Object(
    Map("key1" -> Json.fromInt(3),
        "key2" -> Json.obj("nested" -> Json.fromString("s"), "new" -> Json.True, "deleted" -> Json.Null)))
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
        .through(tokens[Fallible, Char])
        .through(patch(valuePatch))
        .compile
        .toList
    patched shouldBe Right(List(Token.NumberValue("5")))
  }

  it should "replace an object value" in {
    val patched =
      Stream
        .emits("""{"key": 4}""")
        .through(tokens[Fallible, Char])
        .through(patch(valuePatch))
        .compile
        .toList
    patched shouldBe Right(List(Token.NumberValue("5")))
  }

  it should "replace an array value" in {
    val patched =
      Stream
        .emits("""[1, 2, 3]""")
        .through(tokens[Fallible, Char])
        .through(patch(valuePatch))
        .compile
        .toList
    patched shouldBe Right(List(Token.NumberValue("5")))
  }

  "a null patch" should "remove a simple value" in {
    val patched =
      Stream
        .emits("true")
        .through(tokens[Fallible, Char])
        .through(patch(nullPatch))
        .compile
        .toList
    patched shouldBe Right(Nil)
  }

  it should "remove an object value" in {
    val patched =
      Stream
        .emits("""{"key": 4}""")
        .through(tokens[Fallible, Char])
        .through(patch(nullPatch))
        .compile
        .toList
    patched shouldBe Right(Nil)
  }

  it should "remove an array value" in {
    val patched =
      Stream
        .emits("""[1, 2, 3]""")
        .through(tokens[Fallible, Char])
        .through(patch(nullPatch))
        .compile
        .toList
    patched shouldBe Right(Nil)
  }

  "an object patch" should "replace a simple value" in {
    val patched =
      Stream
        .emits("true")
        .through(tokens[Fallible, Char])
        .through(patch(objectPatch))
        .compile
        .toList
    patched shouldBe Right(objectPatchTokens)
  }

  it should "replace an array value" in {
    val patched =
      Stream
        .emits("""[1, 2, 3]""")
        .through(tokens[Fallible, Char])
        .through(patch(objectPatch))
        .compile
        .toList
    patched shouldBe Right(objectPatchTokens)
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
        .through(tokens[Fallible, Char])
        .through(patch(objectPatch))
        .compile
        .toList
    patched shouldBe Right(objectTokens)
  }

}
