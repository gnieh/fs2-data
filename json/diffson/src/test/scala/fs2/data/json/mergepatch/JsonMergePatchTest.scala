/*
 * Copyright 2022 Lucas Satabin
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
package json
package mergepatch

import ast._

import diffson._
import diffson.jsonmergepatch.JsonMergePatch

import weaver._

abstract class JsonMergePatchTest[Json](implicit Json: Jsony[Json], tokenizer: Tokenizer[Json], builder: Builder[Json])
    extends SimpleIOSuite {

  val valuePatch = JsonMergePatch.Value(builder.makeNumber("5"))
  val nullPatch = JsonMergePatch.Value(Json.Null)
  val objectPatch = JsonMergePatch.Object(
    Map(
      "key1" -> builder.makeNumber("3"),
      "key2" -> builder.makeObject(
        List("nested" -> builder.makeString("s"), "new" -> builder.makeTrue, "deleted" -> builder.makeNull))
    ))
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

  pureTest("a value patch should replace a simple value") {
    val patched =
      Stream
        .emit("true")
        .through(tokens[Fallible, String])
        .through(patch(valuePatch))
        .compile
        .toList
    expect(patched == Right(List(Token.NumberValue("5"))))
  }

  pureTest("a value patch should replace an object value") {
    val patched =
      Stream
        .emits("""{"key": 4}""")
        .through(tokens[Fallible, Char])
        .through(patch(valuePatch))
        .compile
        .toList
    expect(patched == Right(List(Token.NumberValue("5"))))
  }

  pureTest("replace an array value") {
    val patched =
      Stream
        .emit("""[1, 2, 3]""")
        .through(tokens[Fallible, String])
        .through(patch(valuePatch))
        .compile
        .toList
    expect(patched == Right(List(Token.NumberValue("5"))))
  }

  pureTest("a null patch should remove a simple value") {
    val patched =
      Stream
        .emit("true")
        .through(tokens[Fallible, String])
        .through(patch(nullPatch))
        .compile
        .toList
    expect(patched == Right(Nil))
  }

  pureTest("a null patch should remove an object value") {
    val patched =
      Stream
        .emit("""{"key": 4}""")
        .through(tokens[Fallible, String])
        .through(patch(nullPatch))
        .compile
        .toList
    expect(patched == Right(Nil))
  }

  pureTest("a null patch should remove an array value") {
    val patched =
      Stream
        .emit("""[1, 2, 3]""")
        .through(tokens[Fallible, String])
        .through(patch(nullPatch))
        .compile
        .toList
    expect(patched == Right(Nil))
  }

  pureTest("an object patch should replace a simple value") {
    val patched =
      Stream
        .emit("true")
        .through(tokens[Fallible, String])
        .through(patch(objectPatch))
        .compile
        .toList
    expect(patched == Right(objectPatchTokens))
  }

  pureTest("an object patch replace an array value") {
    val patched =
      Stream
        .emit("""[1, 2, 3]""")
        .through(tokens[Fallible, String])
        .through(patch(objectPatch))
        .compile
        .toList
    expect(patched == Right(objectPatchTokens))
  }

  pureTest("an object patch be applied recursively to an object value") {

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
        .emit(
          """{"untouched1": 32, "key2": {"nested": "test string", "deleted": false, "untouched2": "another string"}, "key1": true}""")
        .through(tokens[Fallible, String])
        .through(patch(objectPatch))
        .compile
        .toList
    expect(patched == Right(objectTokens))
  }

}
