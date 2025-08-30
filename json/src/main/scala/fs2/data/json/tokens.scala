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

import fs2.Pure
import fs2.data.text.render.{DocEvent, Renderable, Renderer}

import scala.annotation.switch
import scala.annotation.tailrec

sealed abstract class Token(val kind: String) {
  def jsonRepr: String
}

object Token {

  case object StartObject extends Token("object") {
    def jsonRepr: String = "{"
  }
  case object EndObject extends Token("<none>") {
    def jsonRepr: String = "}"
  }

  case object StartArray extends Token("array") {
    def jsonRepr: String = "["
  }
  case object EndArray extends Token("<none>") {
    def jsonRepr: String = "]"
  }

  case class Key(value: String) extends Token("key") {
    def jsonRepr: String = s""""$value""""
  }

  case object NullValue extends Token("null") {
    def jsonRepr: String = "null"
  }

  case object TrueValue extends Token("boolean") {
    def jsonRepr: String = "true"
  }
  case object FalseValue extends Token("boolean") {
    def jsonRepr: String = "false"
  }
  case class NumberValue(value: String) extends Token("number") {
    def jsonRepr: String = value
  }
  case class StringValue(value: String) extends Token("string") {
    def jsonRepr: String = {
      val rendered = new StringBuilder
      rendered.append('"')
      renderString(value, 0, rendered)
      rendered.append('"')
      rendered.result()
    }
  }

  private final val hex = "0123456789abcdef".toArray

  @tailrec
  def renderString(s: String, idx: Int, builder: StringBuilder): Unit =
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
        renderString(s, nextEscape + 1, builder)
      } else {
        // append the rest of the string and we are done
        builder.append(s.substring(idx))
      }
    }

  private val nullValue =
    Stream.emit(DocEvent.Text("null"))

  private val trueValue =
    Stream.emit(DocEvent.Text("true"))

  private val falseValue =
    Stream.emit(DocEvent.Text("false"))

  private final val FirstObjectKey = 0
  private final val ObjectKey = 1
  private final val ObjectValue = 2
  private final val FirstArrayValue = 3
  private final val ArrayValue = 4

  implicit object renderable extends Renderable[Token] {

    private val startObject =
      Stream.emits(DocEvent.GroupBegin :: DocEvent.Text("{") :: Nil)

    private val endEmptyObject =
      Stream.emits(DocEvent.Text("}") :: DocEvent.GroupEnd :: Nil)

    private val endObject =
      Stream.emits(DocEvent.GroupEnd :: DocEvent.IndentEnd :: DocEvent.LineBreak :: Nil) ++ endEmptyObject

    private val startArray =
      Stream.emits(DocEvent.GroupBegin :: DocEvent.Text("[") :: Nil)

    private val endEmptyArray =
      Stream.emits(DocEvent.Text("]") :: DocEvent.GroupEnd :: Nil)

    private val endArray =
      Stream.emits(DocEvent.GroupEnd :: DocEvent.IndentEnd :: DocEvent.LineBreak :: Nil) ++ endEmptyArray

    private val objectSep =
      Stream.emits(DocEvent.Text(",") :: DocEvent.GroupEnd :: DocEvent.Line :: Nil)

    private val arraySep =
      Stream.emits(DocEvent.Text(",") :: DocEvent.GroupEnd :: DocEvent.Line :: DocEvent.GroupBegin :: Nil)

    override def newRenderer(): Renderer[Token] = new Renderer[Token] {

      // the current stack of states, helping to deal with comma and indentation
      // states are described right above
      private[this] var states = List.empty[Int]

      private def separator(): Stream[Pure, DocEvent] =
        states match {
          case Nil =>
            Stream.empty
          case state :: rest =>
            (state: @switch) match {
              case FirstObjectKey =>
                states = ObjectValue :: rest
                Stream.emits(DocEvent.IndentBegin :: DocEvent.LineBreak :: Nil)
              case ObjectKey =>
                states = ObjectValue :: rest
                objectSep
              case ObjectValue =>
                states = ObjectKey :: rest
                Stream.emit(DocEvent.GroupBegin)
              case FirstArrayValue =>
                states = ArrayValue :: rest
                Stream.emits(DocEvent.IndentBegin :: DocEvent.LineBreak :: DocEvent.GroupBegin :: Nil)
              case ArrayValue =>
                states = ArrayValue :: rest
                arraySep
            }
        }

      private def closeObject(): Stream[Pure, DocEvent] =
        states match {
          case Nil           => endEmptyObject
          case state :: rest =>
            states = rest
            (state: @switch) match {
              case FirstObjectKey => endEmptyObject
              case ObjectKey      => endObject
            }
        }

      private def closeArray(): Stream[Pure, DocEvent] =
        states match {
          case Nil           => endEmptyArray
          case state :: rest =>
            states = rest
            (state: @switch) match {
              case FirstArrayValue => endEmptyArray
              case ArrayValue      => endArray
            }
        }

      override def doc(token: Token): Stream[Pure, DocEvent] =
        token match {
          case StartObject =>
            val res = separator() ++ startObject
            states = FirstObjectKey :: states
            res
          case EndObject =>
            closeObject()
          case StartArray =>
            val res = separator() ++ startArray
            states = FirstArrayValue :: states
            res
          case EndArray =>
            closeArray()
          case Key(value) =>
            val rendered = new StringBuilder
            rendered.append('"')
            renderString(value, 0, rendered)
            rendered.append("\": ")
            separator() ++ Stream.emit(DocEvent.Text(rendered.result()))
          case NullValue =>
            separator() ++ nullValue
          case TrueValue =>
            separator() ++ trueValue
          case FalseValue =>
            separator() ++ falseValue
          case NumberValue(value) =>
            separator() ++ Stream.emit(DocEvent.Text(value))
          case StringValue(value) =>
            val rendered = new StringBuilder
            rendered.append('"')
            renderString(value, 0, rendered)
            rendered.append('"')
            separator() ++ Stream.emit(DocEvent.Text(rendered.result()))
        }
    }

  }

  /** A compact version of the JSON rendering with no space or new lines. */
  object compact extends Renderable[Token] {

    private val startObject =
      Stream.emit(DocEvent.Text("{"))

    private val endObject =
      Stream.emit(DocEvent.Text("}"))

    private val startArray =
      Stream.emit(DocEvent.Text("["))

    private val endArray =
      Stream.emit(DocEvent.Text("]"))

    private val sep =
      Stream.emit(DocEvent.Text(","))

    override def newRenderer(): Renderer[Token] = new Renderer[Token] {

      // the current stack of states, helping to deal with comma and indentation
      // states are described right above
      private[this] var states = List.empty[Int]

      private def separator(): Stream[Pure, DocEvent] =
        states match {
          case Nil =>
            Stream.empty
          case state :: rest =>
            (state: @switch) match {
              case FirstObjectKey =>
                states = ObjectValue :: rest
                Stream.empty
              case ObjectKey =>
                states = ObjectValue :: rest
                sep
              case ObjectValue =>
                states = ObjectKey :: rest
                Stream.empty
              case FirstArrayValue =>
                states = ArrayValue :: rest
                Stream.empty
              case ArrayValue =>
                states = ArrayValue :: rest
                sep
            }
        }

      private def closeObject(): Stream[Pure, DocEvent] =
        states match {
          case Nil           => endObject
          case state :: rest =>
            states = rest
            (state: @switch) match {
              case FirstObjectKey => endObject
              case ObjectKey      => endObject
            }
        }

      private def closeArray(): Stream[Pure, DocEvent] =
        states match {
          case Nil           => endArray
          case state :: rest =>
            states = rest
            (state: @switch) match {
              case FirstArrayValue => endArray
              case ArrayValue      => endArray
            }
        }

      override def doc(token: Token): Stream[Pure, DocEvent] =
        token match {
          case StartObject =>
            val res = separator() ++ startObject
            states = FirstObjectKey :: states
            res
          case EndObject =>
            closeObject()
          case StartArray =>
            val res = separator() ++ startArray
            states = FirstArrayValue :: states
            res
          case EndArray =>
            closeArray()
          case Key(value) =>
            val rendered = new StringBuilder
            rendered.append('"')
            renderString(value, 0, rendered)
            rendered.append("\":")
            separator() ++ Stream.emit(DocEvent.Text(rendered.result()))
          case NullValue =>
            separator() ++ nullValue
          case TrueValue =>
            separator() ++ trueValue
          case FalseValue =>
            separator() ++ falseValue
          case NumberValue(value) =>
            separator() ++ Stream.emit(DocEvent.Text(value))
          case StringValue(value) =>
            val rendered = new StringBuilder
            rendered.append('"')
            renderString(value, 0, rendered)
            rendered.append('"')
            separator() ++ Stream.emit(DocEvent.Text(rendered.result()))
        }
    }

  }

}
