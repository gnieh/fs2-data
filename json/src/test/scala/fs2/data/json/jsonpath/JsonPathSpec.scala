/*
 * Copyright 2019-2022 Lucas Satabin
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
package jsonpath

import json.literals._
import jsonpath.literals._

import weaver._

import cats.effect.IO

object JsonPathSpec extends SimpleIOSuite {

  test("simple path") {

    val path = jsonpath"$$..a.c"

    json"""{
             "a": {
               "a": {
                 "c": true
               },
               "b": 1,
               "c": 2
             }
           }"""
      .lift[IO]
      .through(filter.raw(path))
      .parEvalMapUnbounded(_.compile.toList)
      .compile
      .toList
      .map(expect.same(List(List(Token.TrueValue), List(Token.NumberValue("2"))), _))

  }

  val jsonWithArray =
    json"""{
             "a": [
               {"idx": 0},
               {"idx": 1},
               {"idx": 2},
               {"idx": 3},
               {"idx": 4}
             ]
           }"""
      .lift[IO]

  test("array elements") {
    val path = jsonpath"$$.a[3]"

    jsonWithArray
      .through(filter.raw(path))
      .parEvalMapUnbounded(_.compile.toList)
      .compile
      .toList
      .map(expect.same(List(List(Token.StartObject, Token.Key("idx"), Token.NumberValue("3"), Token.EndObject)), _))

  }

  test("array ranges") {
    val path = jsonpath"$$..a[:2]"

    jsonWithArray
      .through(filter.raw(path))
      .parEvalMapUnbounded(_.compile.toList)
      .compile
      .toList
      .map(expect.same(
        List(
          List(Token.StartObject, Token.Key("idx"), Token.NumberValue("0"), Token.EndObject),
          List(Token.StartObject, Token.Key("idx"), Token.NumberValue("1"), Token.EndObject),
          List(Token.StartObject, Token.Key("idx"), Token.NumberValue("2"), Token.EndObject)
        ),
        _
      ))

  }

  test("all array elements") {
    val path = jsonpath"$$.a[*]"

    jsonWithArray
      .through(filter.raw(path))
      .parEvalMapUnbounded(_.compile.toList)
      .compile
      .toList
      .map(expect.same(
        List(
          List(Token.StartObject, Token.Key("idx"), Token.NumberValue("0"), Token.EndObject),
          List(Token.StartObject, Token.Key("idx"), Token.NumberValue("1"), Token.EndObject),
          List(Token.StartObject, Token.Key("idx"), Token.NumberValue("2"), Token.EndObject),
          List(Token.StartObject, Token.Key("idx"), Token.NumberValue("3"), Token.EndObject),
          List(Token.StartObject, Token.Key("idx"), Token.NumberValue("4"), Token.EndObject)
        ),
        _
      ))

  }

  test("single match") {
    val path = jsonpath"$$.a[4]"

    jsonWithArray
      .through(filter.first(path))
      .compile
      .toList
      .map(
        expect.same(
          List(Token.StartObject, Token.Key("idx"), Token.NumberValue("4"), Token.EndObject),
          _
        ))

  }

}
