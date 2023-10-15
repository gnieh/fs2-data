/*
 * Copyright 2023 Lucas Satabin
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
package jq

import cats.effect._
import weaver._

import json.literals._
import literals._

object JqSpec extends SimpleIOSuite {

  val input: Stream[IO, Token] = json"""{
    "a": [
      {"b" : 0},
      {"b" : 1},
      {"b" : 2}
    ]
  }""".lift[IO]

  val compiler = Compiler[IO]

  test("select simple path") {
    for {
      compiled <- compiler.compile(jq".a[0].b")
      result <- input.through(compiled).compile.toList
    } yield expect.same(List(Token.NumberValue("0")), result)
  }

  test("select not found") {
    for {
      compiled <- compiler.compile(jq".a[0].d.e")
      result <- input.through(compiled).compile.toList
    } yield expect.same(Nil, result)
  }

  test("iterate not found") {
    for {
      compiled <- compiler.compile(jq""".d[]""")
      result <- input.through(compiled).compile.toList
    } yield expect.same(Nil, result)
  }

  test("iterate object not found") {
    for {
      compiled <- compiler.compile(jq""".d[] | { "value": .a }""")
      result <- input.through(compiled).compile.toList
    } yield expect.same(Nil, result)
  }

  test("iterate array not found") {
    for {
      compiled <- compiler.compile(jq"""[ .d[] ]""")
      result <- input.through(compiled).compile.toList
    } yield expect.same(List(Token.StartArray, Token.EndArray), result)
  }

  test("simple recursive descent") {
    for {
      compiled <- compiler.compile(jq"..")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartObject,
        Token.Key("a"),
        Token.StartArray,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.EndArray,
        Token.EndObject,
        Token.StartArray,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.EndArray,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.NumberValue("0"),
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.NumberValue("1"),
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.NumberValue("2")
      ),
      result
    )
  }

  test("prefixed recursive descent") {
    for {
      compiled <- compiler.compile(jq".a | ..")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartArray,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.EndArray,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.NumberValue("0"),
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.NumberValue("1"),
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.NumberValue("2")
      ),
      result
    )
  }

  test("identity") {
    for {
      compiled <- compiler.compile(jq".")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartObject,
        Token.Key("a"),
        Token.StartArray,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.EndArray,
        Token.EndObject
      ),
      result
    )
  }

  test("identity noop") {
    for {
      compiled <- compiler.compile(jq".a | . | .[2]")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("2"),
        Token.EndObject
      ),
      result
    )
  }

  test("iterator") {
    for {
      compiled <- compiler.compile(jq".a[]")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("2"),
        Token.EndObject
      ),
      result
    )
  }

  test("object iterator") {
    for {
      compiled <- compiler.compile(jq""".a | {"before": true, "value": .[].b, "after": .[0].b}""")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartObject,
        Token.Key("before"),
        Token.TrueValue,
        Token.Key("value"),
        Token.NumberValue("0"),
        Token.Key("after"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("before"),
        Token.TrueValue,
        Token.Key("value"),
        Token.NumberValue("1"),
        Token.Key("after"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("before"),
        Token.TrueValue,
        Token.Key("value"),
        Token.NumberValue("2"),
        Token.Key("after"),
        Token.NumberValue("0"),
        Token.EndObject
      ),
      result
    )
  }

  test("array iterator") {
    for {
      compiled <- compiler.compile(jq"""[ "before", .a[], "after" ]""")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartArray,
        Token.StringValue("before"),
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.StringValue("after"),
        Token.EndArray
      ),
      result
    )
  }

  test("array iterator with constructor") {
    for {
      compiled <- compiler.compile(jq"""[ "before", .a[] | { "value": .b }, "after" ]""")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartArray,
        Token.StringValue("before"),
        Token.StartObject,
        Token.Key("value"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("value"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("value"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.StringValue("after"),
        Token.EndArray
      ),
      result
    )
  }

  test("object iterator with constructor") {
    for {
      compiled <- compiler.compile(jq"""[ true, .a[].b | {"value": . }, false ]""")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartArray,
        Token.TrueValue,
        Token.StartObject,
        Token.Key("value"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("value"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("value"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.FalseValue,
        Token.EndArray
      ),
      result
    )
  }

  test("object iterator with constructor iterator") {
    for {
      compiled <- compiler.compile(jq""".a[] | { "value": . }""")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartObject,
        Token.Key("value"),
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("0"),
        Token.EndObject,
        Token.EndObject,
        Token.StartObject,
        Token.Key("value"),
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.EndObject,
        Token.StartObject,
        Token.Key("value"),
        Token.StartObject,
        Token.Key("b"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.EndObject
      ),
      result
    )
  }

  test("not found value constructor") {
    for {
      compiled <- compiler.compile(jq"""{ "value": .a[0].d }""")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartObject,
        Token.Key("value"),
        Token.NullValue,
        Token.EndObject
      ),
      result
    )
  }

  test("not found value object iterator") {
    for {
      compiled <- compiler.compile(jq"""{ "value": .a[].unknown }""")
      result <- input.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartObject,
        Token.Key("value"),
        Token.NullValue,
        Token.EndObject,
        Token.StartObject,
        Token.Key("value"),
        Token.NullValue,
        Token.EndObject,
        Token.StartObject,
        Token.Key("value"),
        Token.NullValue,
        Token.EndObject
      ),
      result
    )
  }

  test("documentation") {
    val source = json"""{
      "field1": 0,
      "field2": "test",
      "field3": [1, 2, 3]
    }""".lift[IO]
    for {
      compiled <- compiler.compile(jq"""[ { "field2": .field2, "field3": .field3[] } ]""")
      result <- source.through(compiled).compile.toList
    } yield expect.same(
      List(
        Token.StartArray,
        Token.StartObject,
        Token.Key("field2"),
        Token.StringValue("test"),
        Token.Key("field3"),
        Token.NumberValue("1"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("field2"),
        Token.StringValue("test"),
        Token.Key("field3"),
        Token.NumberValue("2"),
        Token.EndObject,
        Token.StartObject,
        Token.Key("field2"),
        Token.StringValue("test"),
        Token.Key("field3"),
        Token.NumberValue("3"),
        Token.EndObject,
        Token.EndArray
      ),
      result
    )
  }

}
