/*
 * Copyright 2021 Lucas Satabin
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

import ast._
import selector._

import weaver._
import cats.effect.IO

abstract class JsonSelectorSpec[Json](implicit builder: Builder[Json], tokenizer: Tokenizer[Json])
    extends SimpleIOSuite {

  pureTest("mandatory fields should fail in the missing single case") {
    val selector = root.field("field").!.compile
    val filtered = Stream(Token.StartObject, Token.Key("other-field"), Token.TrueValue, Token.EndObject)
      .through(filter[Fallible](selector))
      .compile
      .drain

    expect(
      filtered.left
        .map {
          case e: JsonMissingFieldException => e.missing == Set("field")
          case _                            => false
        }
        .fold(identity, _ => false),
      "Should be Left(JsonMissingFieldException)"
    )
  }

  pureTest("mandatory fields should fail in case at least one is missing") {
    val selector = root.fields("field1", "field2", "field3").!.compile
    val filtered = Stream(Token.StartObject, Token.Key("field2"), Token.TrueValue, Token.EndObject)
      .through(filter[Fallible](selector))
      .compile
      .drain

    expect(
      filtered.left
        .map {
          case e: JsonMissingFieldException => e.missing == Set("field1", "field3")
          case _                            => false
        }
        .fold(identity, _ => false),
      "Should be Left(JsonMissingFieldException)"
    )
  }

  pureTest("mandatory fields should fail in missing nested cases") {
    val selector = root.iterate.field("field").!.compile
    val filtered = Stream(Token.StartArray,
                          Token.StartObject,
                          Token.Key("other-field"),
                          Token.TrueValue,
                          Token.EndObject,
                          Token.EndArray)
      .through(filter[Fallible](selector))
      .compile
      .drain

    expect(
      filtered.left
        .map {
          case e: JsonMissingFieldException => e.missing == Set("field")
          case _                            => false
        }
        .fold(identity, _ => false),
      "Should be Left(JsonMissingFieldException)"
    )
  }

  pureTest("mandatory fields should fail on outermost error in case of nested missing keys") {
    val selector = root.field("field1").!.field("field2").!.compile
    val filtered =
      Stream(Token.StartObject, Token.Key("other-field"), Token.StartObject, Token.EndObject, Token.EndObject)
        .through(filter[Fallible](selector))
        .compile
        .drain

    expect(
      filtered.left
        .map {
          case e: JsonMissingFieldException => e.missing == Set("field1")
          case _                            => false
        }
        .fold(identity, _ => false),
      "Should be Left(JsonMissingFieldException)"
    )
  }

  pureTest("mandatory fields should succeed if all mandatory fields are present") {
    val selector = root.fields("field1", "field2", "field3").!.compile
    val filtered = Stream(
      Token.StartObject,
      Token.Key("field2"),
      Token.TrueValue,
      Token.Key("field1"),
      Token.StringValue("test"),
      Token.Key("other-field"),
      Token.NullValue,
      Token.Key("field3"),
      Token.NumberValue("1"),
      Token.EndObject
    ).through(filter[Fallible](selector)).compile.drain

    expect(filtered == Right(()))
  }

  pureTest("transformOpt should only transform the select element") {
    val selector = root.field("f").compile
    val transformed =
      Stream(Token.StartObject,
             Token.Key("f"),
             Token.TrueValue,
             Token.Key("g"),
             Token.StringValue("test"),
             Token.EndObject)
        .through(transformOpt[Fallible, Json](selector, _ => Some(builder.makeFalse)))
        .compile
        .toList
    expect(
      transformed == Right(
        List(Token.StartObject,
             Token.Key("f"),
             Token.FalseValue,
             Token.Key("g"),
             Token.StringValue("test"),
             Token.EndObject)))
  }

  pureTest("transformOpt should filter out the value if it returns None") {
    val selector = root.field("f").compile
    val transformed =
      Stream(Token.StartObject,
             Token.Key("f"),
             Token.TrueValue,
             Token.Key("g"),
             Token.StringValue("test"),
             Token.EndObject)
        .through(transformOpt[Fallible, Json](selector, _ => None))
        .compile
        .toList
    expect(transformed == Right(List(Token.StartObject, Token.Key("g"), Token.StringValue("test"), Token.EndObject)))
  }

  pureTest("transformOpt should filter out the innermost value if it returns None") {
    val selector = root.field("f").field("g").compile
    val transformed =
      Stream(
        Token.StartObject,
        Token.Key("f"),
        Token.StartObject,
        Token.Key("g"),
        Token.TrueValue,
        Token.EndObject,
        Token.Key("g"),
        Token.StringValue("test"),
        Token.EndObject
      ).through(transformOpt[Fallible, Json](selector, _ => None)).compile.toList
    expect(
      transformed == Right(
        List(Token.StartObject,
             Token.Key("f"),
             Token.StartObject,
             Token.EndObject,
             Token.Key("g"),
             Token.StringValue("test"),
             Token.EndObject)))
  }

  test("transformF should fail the stream if one value fails") {
    val selector = root.field("f").compile
    val exn = new Exception
    val error = new JsonException("An error occurred while transforming Json data",
                                  Some(JsonContext.Key("f", JsonContext.Root)),
                                  exn)
    Stream(Token.StartObject,
           Token.Key("f"),
           Token.TrueValue,
           Token.Key("g"),
           Token.StringValue("test"),
           Token.EndObject)
      .through(transformF[IO, Json](selector, _ => IO.raiseError(exn)))
      .attempt
      .compile
      .toList
      .map(transformed => expect(transformed == List(Right(Token.StartObject), Right(Token.Key("f")), Left(error))))
  }

}
