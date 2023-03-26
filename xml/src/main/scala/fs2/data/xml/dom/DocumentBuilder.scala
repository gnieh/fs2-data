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

package fs2
package data
package xml
package dom

trait DocumentBuilder[Document] extends ElementBuilder {

  def makeDocument(version: Option[String],
                   encoding: Option[String],
                   standalone: Option[Boolean],
                   doctype: Option[XmlEvent.XmlDoctype],
                   prolog: List[Misc],
                   root: Elem,
                   postlog: List[Misc]): Document

}

object DocumentBuilder {
  type Aux[D, C, E <: C] = DocumentBuilder[D] {
    type Content = C
    type Elem = E
  }
}
