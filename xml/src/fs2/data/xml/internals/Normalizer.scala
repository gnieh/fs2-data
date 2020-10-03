/*
 * Copyright 2019 Lucas Satabin
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

import cats.implicits._

import fs2._

private[xml] object Normalizer {

  def pipe[F[_]]: Pipe[F, XmlEvent, XmlEvent] =
    _.groupAdjacentBy {
      case XmlEvent.XmlString(_, isCdata) => !isCdata
      case _                              => false
    }.flatMap {
      case (false, chunk) => Stream.chunk(chunk.map(mergeAttributes))
      case (true, texts) =>
        Stream.emit(
          XmlEvent.XmlString(texts
                               .collect { case XmlEvent.XmlString(s, _) =>
                                 s
                               }
                               .iterator
                               .mkString,
                             false))
    }

  private def mergeAttributes(evt: XmlEvent): XmlEvent =
    evt match {
      case evt @ XmlEvent.StartTag(_, attrs, _) => evt.copy(attributes = attrs.map(mergeAttribute))
      case _                                    => evt
    }

  private def mergeAttribute(a: Attr): Attr = {
    def loop(values: List[XmlEvent.XmlTexty],
             acc: List[XmlEvent.XmlTexty],
             current: Option[StringBuilder]): List[XmlEvent.XmlTexty] =
      values match {
        case Nil =>
          current match {
            case Some(sb) => (XmlEvent.XmlString(sb.result(), false) :: acc).reverse
            case None     => acc.reverse
          }
        case XmlEvent.XmlString(s, false) :: tail =>
          val sb = current.getOrElse(new StringBuilder)
          loop(tail, acc, Some(sb.append(s)))
        case evt :: tail =>
          current match {
            case Some(sb) => loop(tail, evt :: XmlEvent.XmlString(sb.result(), false) :: acc, None)
            case None     => loop(tail, evt :: acc, None)
          }
      }
    a.copy(value = loop(a.value, Nil, None))
  }

}
