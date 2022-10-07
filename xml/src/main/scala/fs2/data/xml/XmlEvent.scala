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
package xml

import cats.Show
import cats.syntax.all._

sealed trait XmlEvent

object XmlEvent {

  sealed trait XmlTexty extends XmlEvent {
    def render: String
  }

  case object StartDocument extends XmlEvent

  case class XmlDecl(version: String, encoding: Option[String], standalone: Option[Boolean]) extends XmlEvent

  case class StartTag(name: QName, attributes: List[Attr], isEmpty: Boolean) extends XmlEvent {
    def render(collapseEmpty: Boolean): String = {
      val end = if (collapseEmpty && isEmpty) "/>" else ">"
      val attrs = attributes.foldMap[String] { case Attr(n, v) => show""" $n="${v.foldMap[String](_.render)}"""" }
      show"""<$name$attrs$end"""
    }
  }

  case class XmlCharRef(value: Int) extends XmlTexty {
    def render = f"&#x$value%04x;"
  }

  case class XmlEntityRef(name: String) extends XmlTexty {
    def render = s"&$name;"
  }

  case class XmlString(s: String, isCDATA: Boolean) extends XmlTexty {
    def render = if (isCDATA) show"<![CDATA[$s]]>" else s
  }

  case class XmlPI(target: String, content: String) extends XmlEvent

  case class XmlDoctype(name: String, docname: String, systemid: Option[String]) extends XmlEvent

  case class EndTag(name: QName) extends XmlEvent

  case object EndDocument extends XmlEvent

  case class Comment(comment: String) extends XmlEvent

  implicit val show: Show[XmlEvent] = Show.show {
    case t: XmlTexty      => t.render
    case s: StartTag      => s.render(false)
    case EndTag(n)        => show"</$n>"
    case Comment(content) => s"<!--$content-->"
    case XmlDecl(version, encoding, standalone) =>
      s"""<?xml version="$version"${encoding.foldMap(e => s""" encoding="$e"""")}${standalone.foldMap {
          case true  => s""" standalone="yes""""
          case false => s""" standalone="no""""
        }}?>"""
    case XmlPI(target, content) => s"<?$target $content?>"
    case _                      => ""
  }

}
