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
package dom

trait Builder[Node] {

  def makeDocument(version: Option[String],
                   encoding: Option[String],
                   standalone: Option[Boolean],
                   doctype: Option[XmlEvent.XmlDoctype],
                   prolog: List[Node],
                   root: Node): Node

  def makeComment(content: String): Option[Node]

  def makeText(texty: XmlEvent.XmlTexty): Node

  def makeElement(name: QName, attributes: List[Attr], isEmpty: Boolean, children: List[Node]): Node

  def makePI(target: String, content: String): Node

}
