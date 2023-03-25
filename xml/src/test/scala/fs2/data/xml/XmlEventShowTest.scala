/*
 * Copyright 2023 Lucas Satabin
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

package fs2.data.xml

import cats.Show
import cats.syntax.all._
import weaver._

object XmlEventShowTest extends SimpleIOSuite {

  implicit def showX[X <: XmlEvent]: Show[X] = XmlEvent.show.narrow

  import XmlEvent._

  pureTest("texts") {
    expect.eql("foo", XmlString("foo", false).show) &&
    expect.eql("<![CDATA[foo]]>", XmlString("foo", true).show) &&
    expect.eql("&#x04d2;", XmlCharRef(0x04d2).show) &&
    expect.eql("&amp;", XmlEntityRef("amp").show)
  }

  pureTest("tags") {
    val hello = QName("hello")
    val preHello = QName(Some("pre"), "hello")
    val attrA = Attr(QName("a"), List(XmlString("1", false)))
    val attrB = Attr(QName("b"), List(XmlString("2", false)))

    expect.eql("<hello>", StartTag(hello, Nil, false).show) &&
    expect.eql("<hello>", StartTag(hello, Nil, true).show) && // ideally, this would use />
    expect.eql("<pre:hello>", StartTag(preHello, Nil, false).show) &&
    expect.eql("<hello a=\"1\">", StartTag(hello, List(attrA), false).show) &&
    expect.eql("<hello a=\"1\" b=\"2\">", StartTag(hello, List(attrA, attrB), false).show) &&
    expect.eql("</hello>", EndTag(hello).show)
  }

  pureTest("comments") {
    expect.eql("<!--something-->", Comment("something").show)
  }

  pureTest("declarations") {
    expect.eql("<?xml version=\"1.0\"?>", XmlDecl("1.0", None, None).show) &&
    expect.eql("<?xml version=\"1.0\" encoding=\"utf-8\"?>", XmlDecl("1.0", Some("utf-8"), None).show) &&
    expect.eql("<?xml version=\"1.0\" standalone=\"yes\"?>", XmlDecl("1.0", None, Some(true)).show) &&
    expect.eql("<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>",
               XmlDecl("1.0", Some("utf-8"), Some(true)).show)
  }

  pureTest("pi") {
    expect.eql("<?target content?>", XmlPI("target", "content").show)
  }

}
