/*
 * Copyright 2022 Lucas Satabin
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

import dom.Builder

import cats.syntax.all._

import scala.xml._

package object scalaXml {
  implicit object ScalaXmlBuilder extends Builder[NodeSeq] {

    def makeDocument(version: Option[String],
                     encoding: Option[String],
                     standalone: Option[Boolean],
                     doctype: Option[XmlEvent.XmlDoctype],
                     prolog: List[NodeSeq],
                     root: NodeSeq): NodeSeq = {
      val document = new Document()
      document.version = version
      document.encoding = encoding
      document.standAlone = standalone
      document.children = prolog.flatten ++ root
      document.docElem = root.head
      document
    }

    def makeComment(content: String): Option[NodeSeq] =
      Comment(content).some

    def makeText(texty: XmlEvent.XmlTexty): NodeSeq =
      texty match {
        case XmlEvent.XmlCharRef(value)   => Text(new String(Character.toChars(value)))
        case XmlEvent.XmlEntityRef(name)  => EntityRef(name)
        case XmlEvent.XmlString(s, false) => Text(s)
        case XmlEvent.XmlString(s, true)  => PCData(s)
      }

    def makeElement(name: QName, attributes: List[Attr], isEmpty: Boolean, children: List[NodeSeq]): NodeSeq = {
      val attrs = attributes.foldRight(Null: MetaData) { (attr, acc) =>
        attr.name.prefix match {
          case Some(prefix) => new PrefixedAttribute(prefix, attr.name.local, attr.value.flatMap(makeText(_)), acc)
          case None         => new UnprefixedAttribute(attr.name.local, attr.value.flatMap(makeText(_)), acc)
        }
      }
      Elem(name.prefix.getOrElse(null), name.local, attrs, TopScope, isEmpty, children.flatten: _*)
    }

    def makePI(target: String, content: String): NodeSeq =
      ProcInstr(target, content)

  }
}
