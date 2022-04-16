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
package fs2
package data
package json
package internals

import scala.annotation.{tailrec, switch}

private[json] class Renderer(pretty: Boolean, resetOnChunk: Boolean, indent: String)
    extends Collector.Builder[Token, String] {

  import Renderer._

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
    @tailrec
    def loop(idx: Int): Unit =
      if (idx < s.length) {
        val nextEscape = s.indexWhere(c => c > 127 || Character.isISOControl(c) || "\\/\b\f\n\r\t\"".contains(c), idx)
        if (nextEscape >= 0) {
          if (nextEscape > 0) {
            builder.append(s.substring(idx, nextEscape))
          }
          val c = s(nextEscape)
          (c: @switch) match {
            case '\\' =>
              builder.append("\\\\")
            case '/' =>
              builder.append("\\/")
            case '\b' =>
              builder.append("\\b")
            case '\f' =>
              builder.append("\\f")
            case '\n' =>
              builder.append("\\n")
            case '\r' =>
              builder.append("\\r")
            case '\t' =>
              builder.append("\\t")
            case '"' =>
              builder.append("\\\"")
            case _ =>
              // escape non ascii or control characters
              builder
                .append("\\u")
                .append(hex((c >> 12) & 0x0f))
                .append(hex((c >> 8) & 0x0f))
                .append(hex((c >> 4) & 0x0f))
                .append(hex(c & 0x0f))
          }
          loop(nextEscape + 1)
        } else {
          // append the rest of the string and we are done
          builder.append(s.substring(idx))
        }
      }

    prefixValue()
    builder.append('"')
    loop(0)
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

  private val hex = "0123456789abcdef"

  def pipe[F[_]](pretty: Boolean, indent: String): Pipe[F, Token, String] =
    in =>
      Stream.suspend(Stream.emit(new Renderer(pretty, true, indent))).flatMap { builder =>
        in.mapChunks { chunk =>
          builder += chunk
          Chunk.singleton(builder.result)
        }
      }

}
