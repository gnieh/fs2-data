/*
 * Copyright 2020 Lucas Satabin
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

import fs2._

import circe._

import cats.implicits._

import _root_.io.circe.Json

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JsonExceptionSpec extends AnyFlatSpec with Matchers {

  "previous valid tokens" should "be emitted before Exception" in {

    val input = """{"key": }"""

    val stream = Stream.emit(input).through(tokens[Fallible, String]).attempt

    stream.compile.toList should matchPattern {
      case Right(List(Right(Token.StartObject), Right(Token.Key("key")), Left(_: JsonException))) =>
    }

  }

  "previous selected tokens" should "be emitted before Exception" in {

    val input = """{"key1": 1}[]"""

    val selector = ".key1".parseSelector[Either[Throwable, *]].fold(throw _, identity)
    val stream = Stream.emit(input).through(tokens[Fallible, String]).through(filter(selector)).attempt

    stream.compile.toList should matchPattern {
      case Right(List(Right(Token.NumberValue("1")), Left(_: JsonException))) =>
    }

  }

  "previous valid values" should "be emitted before Exception" in {

    val input = """{"key": "value"}[1,"""

    val stream = Stream.emit(input).through(tokens[Fallible, String]).through(values).attempt

    stream.compile.toList should matchPattern {
      case Right(List(Right(o), Left(_: JsonException))) if o == Json.obj("key" -> Json.fromString("value")) =>
    }

  }

}
