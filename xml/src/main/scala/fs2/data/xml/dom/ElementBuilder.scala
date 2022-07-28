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
package dom

trait ElementBuilder {

  type Content
  type Misc <: Content
  type Elem <: Content

  def makeComment(content: String): Option[Misc]

  def makeText(texty: XmlEvent.XmlTexty): Content

  def makeElement(name: QName, attributes: List[Attr], isEmpty: Boolean, children: List[Content]): Elem

  def makePI(target: String, content: String): Misc

}

object ElementBuilder {

  type Aux[E] = ElementBuilder { type Elem = E }

}
