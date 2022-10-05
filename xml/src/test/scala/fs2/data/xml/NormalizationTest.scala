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

import fs2._

import weaver._

object NormalizationTest extends SimpleIOSuite {

  def start(name: String) = XmlEvent.StartTag(QName(name), Nil, false)
  def text(s: String) = XmlEvent.XmlString(s, false)

  pureTest("consecutive non CDATA string should be  merged") {
    val input = Stream.emits(
      List(XmlEvent.XmlString("This", false),
           XmlEvent.XmlString(" is ", false),
           XmlEvent.XmlString("a string.", false)))

    val actual = input.through(normalize).compile.toList

    expect(actual == List(XmlEvent.XmlString("This is a string.", false)))

  }

  pureTest("consecutive non CDATA string should be merged no matter how deep it is") {
    val input = Stream.emits(
      List(start("test"), start("test"), text("Text"), text("."), start("test"), text("More "), text("text")))

    val actual = input.through(normalize).compile.toList

    expect(actual == List(start("test"), start("test"), text("Text."), start("test"), text("More text")))
  }

  pureTest("consecutive non CDATA string should also be merged in attribute values") {
    val input = Stream.emits(
      List(
        XmlEvent.StartTag(
          QName("test"),
          List(Attr(QName("a"), List(XmlEvent.XmlString("attribute ", false), XmlEvent.XmlString("value", false)))),
          true)))

    val actual = input.through(normalize).compile.toList

    expect(
      actual == List(XmlEvent
        .StartTag(QName("test"), List(Attr(QName("a"), List(XmlEvent.XmlString("attribute value", false)))), true)))
  }

  pureTest("mixed CDATA and non CDATA strings should not be merged") {
    val input =
      List(XmlEvent.XmlString("This", false), XmlEvent.XmlString(" is ", true), XmlEvent.XmlString("a string.", false))

    val actual = Stream.emits(input).through(normalize).compile.toList

    expect(actual == input)

  }

}
