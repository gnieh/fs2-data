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

import xml.internals._

import cats._

package object xml {

  /** The predefined XML character entities
    */
  val xmlEntities = Map("quot" -> "\"", "amp" -> "&", "apos" -> "'", "lt" -> "<", "gt" -> ">")

  /** Transforms a stream of characters into a stream of XML events.
    * Emitted tokens are guaranteed to be valid up to that point.
    * If the streams ends without failure, the sequence of tokens is sensured
    * to represent a (potentially empty) sequence of valid XML documents.
    */
  def events[F[_]](implicit F: ApplicativeError[F, Throwable]): Pipe[F, Char, XmlEvent] =
    EventParser.pipe[F]

  /** Resolves the character and entity references in the XML stream.
    * Entities are already defined and validated (especially no recursion),
    * hence the `entities` map contains the resolved text to replace the entity by.
    */
  def referenceResolver[F[_]](entities: Map[String, String] = xmlEntities)(
      implicit F: MonadError[F, Throwable]): Pipe[F, XmlEvent, XmlEvent] =
    ReferenceResolver[F](entities).pipe

  /** Resolves all prefixes in [[QName]]s.
    * Assumes that entity and character references have been resolved
    * already. Make stream go through [[referenceResolver]] first if you
    * need it (not needed if you xml doesn't contain any such reference).
    */
  def namespaceResolver[F[_]](implicit F: MonadError[F, Throwable]): Pipe[F, XmlEvent, XmlEvent] =
    NamespaceResolver[F].pipe

  /** Performs some event normalizations:
    *  - group consecutive non CDATA [[XmlEvent.XmlString]]s
    * This can be useful to merge texts once references have been resolved.
    * Attribute values are also normalized, so that they will end up being
    * one single string after normalization if references have been replaced.
    */
  def normalize[F[_]]: Pipe[F, XmlEvent, XmlEvent] =
    Normalizer.pipe[F]

}
