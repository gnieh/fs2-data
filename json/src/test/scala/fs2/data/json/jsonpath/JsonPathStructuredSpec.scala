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

package fs2.data.json.jsonpath

import fs2.data.json.literals._
import fs2.data.json.jsonpath.literals._
import weaver._
import cats.effect.IO
import fs2.data.json.ast.Builder
import fs2.data.json.codec.Deserializer

abstract class JsonPathStructuredSpec[Json] extends SimpleIOSuite {

  implicit def builder: Builder[Json]
  implicit def deserializer: Deserializer[Data]

  val recursivePath = jsonpath"$$..a"

  val recursiveJson =
    json"""{
             "a": {
               "a": {
                 "value": true
               },
               "value": 2
             }
           }"""
      .lift[IO]

  test("deterministic AST") {
    recursiveJson
      .through(filter.values(recursivePath))
      .compile
      .toList
      .map(expect.same(
        List(
          builder.makeObject(
            List("a" -> builder.makeObject(List("value" -> builder.makeTrue)), "value" -> builder.makeNumber("2"))),
          builder.makeObject(List("value" -> builder.makeTrue))
        ),
        _
      ))
  }

  test("early AST") {
    recursiveJson
      .through(filter.values(recursivePath, deterministic = false))
      .compile
      .toList
      .map(expect.same(
        List(
          builder.makeObject(List("value" -> builder.makeTrue)),
          builder.makeObject(
            List("a" -> builder.makeObject(List("value" -> builder.makeTrue)), "value" -> builder.makeNumber("2")))
        ),
        _
      ))
  }

  test("deterministic deserialized") {
    recursiveJson
      .through(filter.deserialize(recursivePath))
      .compile
      .toList
      .map(expect.same(List(Data.Number(2), Data.Bool(true)), _))
  }

  test("early deserialized") {
    recursiveJson
      .through(filter.deserialize[Data](recursivePath, deterministic = false))
      .compile
      .toList
      .map(expect.same(List(Data.Bool(true), Data.Number(2)), _))
  }

}

sealed trait Data
object Data {
  final case class Number(value: Int) extends Data
  final case class Bool(value: Boolean) extends Data
}
