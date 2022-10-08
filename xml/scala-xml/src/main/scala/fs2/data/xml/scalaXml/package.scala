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

import dom.{DocumentBuilder, DocumentEventifier}

import cats.syntax.all._

import scala.xml._

package object scalaXml {

  implicit val ScalaXmlBuilder: DocumentBuilder.Aux[Document, Node, Elem] = new DocumentBuilder[Document] {

    type Content = Node
    type Elem = scala.xml.Elem
    type Misc = Node

    def makeDocument(version: Option[String],
                     encoding: Option[String],
                     standalone: Option[Boolean],
                     doctype: Option[XmlEvent.XmlDoctype],
                     prolog: List[Misc],
                     root: Elem,
                     postlog: List[Misc]): Document = {
      val document = new Document()
      document.version = version
      document.encoding = encoding
      document.standAlone = standalone
      document.children = prolog ++ (root :: postlog)
      document.docElem = root.head
      document
    }

    def makeComment(content: String): Option[Misc] =
      Comment(content).some

    def makeText(texty: XmlEvent.XmlTexty): Content =
      texty match {
        case XmlEvent.XmlCharRef(value)   => Text(new String(Character.toChars(value)))
        case XmlEvent.XmlEntityRef(name)  => EntityRef(name)
        case XmlEvent.XmlString(s, false) => Text(s)
        case XmlEvent.XmlString(s, true)  => PCData(s)
      }

    def makeElement(name: QName, attributes: List[Attr], isEmpty: Boolean, children: List[Content]): Elem = {
      val attrs = attributes.foldRight(Null: MetaData) { (attr, acc) =>
        attr.name.prefix match {
          case Some(prefix) => new PrefixedAttribute(prefix, attr.name.local, attr.value.map(makeText(_)), acc)
          case None         => new UnprefixedAttribute(attr.name.local, attr.value.map(makeText(_)), acc)
        }
      }
      Elem(name.prefix.getOrElse(null), name.local, attrs, TopScope, isEmpty, children: _*)
    }

    def makePI(target: String, content: String): Misc =
      ProcInstr(target, content)

  }

  implicit object ScalaXmlEventifier extends DocumentEventifier[Document] {

    def eventify(node: Document): Stream[Pure, XmlEvent] =
      innerEventify(node)

    def innerEventify(node: NodeSeq): Stream[Pure, XmlEvent] =
      node match {
        case Comment(comment)           => Stream.emit(XmlEvent.Comment(comment))
        case PCData(content)            => Stream.emit(XmlEvent.XmlString(content, true))
        case Text(content)              => Stream.emit(XmlEvent.XmlString(content, false))
        case EntityRef(name)            => Stream.emit(XmlEvent.XmlEntityRef(name))
        case ProcInstr(target, content) => Stream.emit(XmlEvent.XmlPI(target, content))
        case e: Elem =>
          val isEmpty = e.minimizeEmpty && e.child.isEmpty
          val name = QName(Option(e.prefix), e.label)
          Stream.emit(XmlEvent.StartTag(name, makeAttributes(e.attributes, Nil), isEmpty)) ++ Stream
            .emits(e.child)
            .flatMap(innerEventify(_)) ++ Stream.emit(XmlEvent.EndTag(name))
        case doc: Document =>
          Stream.emit(XmlEvent.StartDocument) ++ Stream.emits(
            doc.version.map(version => XmlEvent.XmlDecl(version, doc.encoding, doc.standAlone)).toSeq) ++ Stream
            .emits(doc.children)
            .flatMap(innerEventify(_)) ++ Stream.emit(XmlEvent.EndDocument)
        case Group(children) => Stream.emits(children).flatMap(innerEventify(_))
        case _               => Stream.empty
      }

    private def makeTexty(nodes: List[Node]): List[XmlEvent.XmlTexty] =
      nodes.collect {
        case Text(s)         => XmlEvent.XmlString(s, false)
        case EntityRef(name) => XmlEvent.XmlEntityRef(name)
      }

    private def makeAttributes(md: MetaData, acc: List[Attr]): List[Attr] =
      md match {
        case Null => acc.reverse
        case PrefixedAttribute(prefix, key, value, next) =>
          makeAttributes(next, Attr(QName(Option(prefix), key), makeTexty(value.toList)) :: acc)
        case UnprefixedAttribute(key, value, next) =>
          makeAttributes(next, Attr(QName(key), makeTexty(value.toList)) :: acc)
      }
  }
}
