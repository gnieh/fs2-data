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
package xpath
package internals

import pfsa._

import cats.effect.Concurrent
import cats.syntax.all._

private[xpath] class XmlQueryPipe[F[_]: Concurrent](dfa: PDFA[LocationMatch, StartElement])
    extends TreeQueryPipe[F, XmlEvent, XmlEvent.StartTag, LocationMatch, StartElement](dfa) {

  override def makeMatchingElement(tok: XmlEvent.StartTag): StartElement =
    StartElement(tok.name, resolveAttr(tok.attributes))

  override def isOpen(tok: XmlEvent): Option[XmlEvent.StartTag] =
    tok match {
      case tok @ XmlEvent.StartTag(_, _, _) => tok.some
      case _                                => none
    }

  override def isClose(tok: XmlEvent): Boolean =
    tok match {
      case tok @ XmlEvent.EndTag(_) => true
      case _                        => false
    }

  private def resolveAttr(attrs: List[Attr]): Map[QName, String] =
    attrs.map { case Attr(n, v) => (n, v.widen[XmlEvent].mkString_("")) }.toMap

}
