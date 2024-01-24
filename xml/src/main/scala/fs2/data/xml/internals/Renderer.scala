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
package xml
package internals

import cats.syntax.all._

private[xml] class Renderer(pretty: Boolean,
                            collapseEmpty: Boolean,
                            resetOnChunk: Boolean,
                            indent: String,
                            attributeThreshold: Int)
    extends Collector.Builder[XmlEvent, String] {

  private val builder = new StringBuilder

  private var level = 0

  private var newline = false

  private var skipClose = false

  private def indentation(): Unit =
    if (pretty && newline) {
      builder.append('\n')
      builder.append(indent * level)
    }

  override def +=(chunk: Chunk[XmlEvent]): Unit = {
    if (resetOnChunk)
      builder.setLength(0)
    chunk.foreach {
      case e @ (XmlEvent.XmlDecl(_, _, _) | XmlEvent.XmlPI(_, _)) =>
        indentation()
        builder ++= e.show
        newline = true

      case XmlEvent.Comment(content) =>
        newline = true
        indentation()
        builder ++= "<!--"
        content.linesIterator.foreach { line =>
          indentation()
          builder ++= line.trim()
        }
        indentation()
        builder ++= "-->"

      case XmlEvent.StartTag(name, attributes, isEmpty) =>
        indentation()
        val renderedName = name.show
        builder ++= show"<$renderedName"

        attributes match {
          case a :: as =>
            val exceedThreshold = as.size > attributeThreshold - 1
            builder ++= show" $a"
            as.foreach { a =>
              if (exceedThreshold) {
                builder += '\n'
                builder ++= " " * (renderedName.length() + 2)
              } else {
                builder += ' '
              }
              builder ++= a.show
            }
          case Nil => // do nothing
        }

        if (isEmpty && collapseEmpty) {
          if (pretty)
            builder ++= " />"
          else
            builder ++= "/>"
          skipClose = true
        } else {
          builder += '>'
          level += 1
        }
        newline = true

      case XmlEvent.EndTag(name) =>
        newline = true
        if (!skipClose) {
          level -= 1
          indentation()
          builder ++= show"</$name>"
        }
        skipClose = false

      case XmlEvent.XmlString(content, true) =>
        indentation()
        builder ++= show"<![CDATA[$content]]>"
        newline = true

      case XmlEvent.XmlString(content, false) if pretty =>
        content.linesIterator.foreach { line =>
          if (line.matches("\\s*")) {
            // empty line, ignore it
          } else {
            indentation()
            if (newline)
              builder ++= line.stripLeading()
            else
              builder ++= line
            newline = true
          }
        }
        newline = content.matches("^.*\n\\s*$")

      case XmlEvent.StartDocument | XmlEvent.EndDocument =>
      // do nothing
      case e =>
        indentation()
        builder ++= e.show
        newline = false
    }
  }

  override def result: String = builder.result()

}

private[xml] object Renderer {

  def pipe[F[_]](pretty: Boolean,
                 collapseEmpty: Boolean,
                 indent: String,
                 attributeThreshold: Int): Pipe[F, XmlEvent, String] =
    in =>
      Stream.suspend(Stream.emit(new Renderer(pretty, collapseEmpty, true, indent, attributeThreshold))).flatMap {
        builder =>
          in.mapChunks { chunk =>
            builder += chunk
            Chunk.singleton(builder.result)
          }

      }

}
