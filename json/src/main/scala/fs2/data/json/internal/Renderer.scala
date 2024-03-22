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
package json
package internals

private[json] class Renderer(pretty: Boolean, resetOnChunk: Boolean, indent: String)
    extends Collector.Builder[Token, String] {

  private val builder = new StringBuilder

  private var level = 0

  private var newline = false

  private var comma = false

  private def indentation(newline: Boolean, level: Int): Unit =
    if (newline) {
      builder.append('\n')
      builder.append(indent * level)
    }

  private def jsonString(s: String, key: Boolean): Unit = {
    prefixValue()
    builder.append('"')
    Token.renderString(s, 0, builder)
    builder.append('"')

    if (key) {
      newline = false
      comma = false
    } else {
      newline = true
      comma = level > 0
    }
  }

  private def prefixValue(): Unit = {
    if (comma)
      builder.append(',')
    if (pretty)
      indentation(newline, level)
  }

  private def jsonValue(repr: String): Unit = {
    prefixValue()
    builder.append(repr)

    newline = true
    comma = level > 0
  }

  def +=(chunk: Chunk[Token]): Unit = {
    if (resetOnChunk)
      builder.setLength(0)
    chunk.foreach {
      case Token.StartObject =>
        prefixValue()
        builder.append('{')

        level += 1
        newline = true
        comma = false
      case Token.EndObject =>
        if (pretty && comma)
          indentation(newline, level - 1)

        builder.append('}')

        level -= 1
        newline = true
        comma = level > 0
      case Token.StartArray =>
        prefixValue()
        builder.append('[')

        level += 1
        newline = true
        comma = false
      case Token.EndArray =>
        if (pretty && comma)
          indentation(newline, level - 1)

        builder.append(']')

        level -= 1
        newline = true
        comma = level > 0
      case Token.Key(key) =>
        jsonString(key, true)
        builder.append(':')
        if (pretty)
          builder.append(' ')
      case Token.TrueValue =>
        jsonValue("true")
      case Token.FalseValue =>
        jsonValue("false")
      case Token.NullValue =>
        jsonValue("null")
      case Token.NumberValue(repr) =>
        jsonValue(repr)
      case Token.StringValue(s) =>
        jsonString(s, false)
    }
  }

  def result: String = builder.result()

}

private[json] object Renderer {

  def pipe[F[_]](pretty: Boolean, indent: String): Pipe[F, Token, String] =
    in =>
      Stream.suspend(Stream.emit(new Renderer(pretty, true, indent))).flatMap { builder =>
        in.mapChunks { chunk =>
          builder += chunk
          Chunk.singleton(builder.result)
        }
      }

}
