/*
 * Copyright 2024 fs2-data Project
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

import cats._
import cats.syntax.all._

private[xml] class ReferenceResolver[F[_]](entities: Map[String, String])(implicit F: MonadError[F, Throwable]) {

  def pipe: Pipe[F, XmlEvent, XmlEvent] =
    _.evalMap[F, XmlEvent] {
      case evt @ XmlEvent.StartTag(_, attrs, _) =>
        for {
          attrs <- attrs.traverse(attr =>
            resolve(attr.value).map(v => attr.copy(value = List(XmlEvent.XmlString(v, false)))))
        } yield evt.copy(attributes = attrs)
      case ref @ XmlEvent.XmlCharRef(_) =>
        resolve(List(ref)).map(XmlEvent.XmlString(_, false))
      case ref @ XmlEvent.XmlEntityRef(_) =>
        resolve(List(ref)).map(XmlEvent.XmlString(_, false))
      case evt =>
        F.pure(evt)
    }

  private def resolve(textys: List[XmlEvent.XmlTexty]): F[String] =
    textys
      .foldM(new StringBuilder) {
        case (acc, XmlEvent.XmlCharRef(n)) =>
          F.pure(acc.append(new String(Character.toChars(n))))
        case (acc, XmlEvent.XmlEntityRef(name)) =>
          entities.get(name) match {
            case Some(value) =>
              F.pure(acc.append(value))
            case None =>
              F.raiseError[StringBuilder](new XmlException(WFCEntityDeclared, s"undeclared entity $name"))
          }
        case (acc, XmlEvent.XmlString(v, _)) =>
          F.pure(acc.append(v))
      }
      .map(_.result())

}

private[xml] object ReferenceResolver {
  def apply[F[_]](entities: Map[String, String])(implicit F: MonadError[F, Throwable]): ReferenceResolver[F] =
    new ReferenceResolver[F](entities)
}
