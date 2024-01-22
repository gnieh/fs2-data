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

import text._
import xml.internals._

import cats._
import cats.syntax.all._

package object xml {

  /** The predefined XML character entities
    */
  val xmlEntities = Map("quot" -> "\"", "amp" -> "&", "apos" -> "'", "lt" -> "<", "gt" -> ">")

  /** Transforms a stream of characters into a stream of XML events.
    * Emitted tokens are guaranteed to be valid up to that point.
    * If the streams ends without failure, the sequence of tokens is ensured
    * to represent a (potentially empty) sequence of valid XML documents.
    */
  @deprecated(message = "Use `fs2.data.xml.events()` instead.", since = "fs2-data 1.4.0")
  def events[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): Pipe[F, T, XmlEvent] =
    EventParser.pipe[F, T](false)

  /** Transforms a stream of characters into a stream of XML events.
    * Emitted tokens are guaranteed to be valid up to that point.
    * If the streams ends without failure, the sequence of tokens is ensured
    * to represent a (potentially empty) sequence of valid XML documents.
    *
    * if `includeComments` is `true`, then comment events will be emitted
    * together with the comment content.
    */
  def events[F[_], T](
      includeComments: Boolean = false)(implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): Pipe[F, T, XmlEvent] =
    EventParser.pipe[F, T](includeComments)

  /** Resolves the character and entity references in the XML stream.
    * Entities are already defined and validated (especially no recursion),
    * hence the `entities` map contains the resolved text to replace the entity by.
    */
  def referenceResolver[F[_]](entities: Map[String, String] = xmlEntities)(implicit
      F: MonadError[F, Throwable]): Pipe[F, XmlEvent, XmlEvent] =
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

  /**
    * Render the incoming xml events to their string representation. The output will be concise,
    * without additional (or original) whitespace and with empty tags being collapsed to the short self-closed form <x/>
    * if collapseEmpty is true. Preserves chunking, each String in the output will correspond to one event in the input.
    */
  @deprecated(message = "Use `fs2.data.xml.render.raw() instead.`", since = "fs2-data 1.11.0")
  def render[F[_]](collapseEmpty: Boolean = true): Pipe[F, XmlEvent, String] =
    render.raw(collapseEmpty)

  object render {

    /**
    * Render the incoming xml events to their string representation. The output will be concise,
    * without additional (or original) whitespace and with empty tags being collapsed to the short self-closed form <x/>
    * if collapseEmpty is true. Preserves chunking, each String in the output will correspond to one event in the input.
    */
    def raw[F[_]](collapseEmpty: Boolean = true): Pipe[F, XmlEvent, String] =
      _.zipWithPrevious.map {
        case (_, st: XmlEvent.StartTag)                                                 => st.render(collapseEmpty)
        case (Some(XmlEvent.StartTag(_, _, true)), XmlEvent.EndTag(_)) if collapseEmpty => ""
        case (_, event)                                                                 => event.show
      }

    /**
      * Render the incoming xml events intot a prettified string representation.
      * _Prettified_ means that nested tags will be indented as per `indent` parameter
      * and text data (except for `CDATA`, which remains untouched) is indented to the current
      * indentation level after each new line.
      *
      * This pipe can be used when whitespace characters are not relevant to the application
      * and to make it more readable to human beings.
      *
      * @param collapseEmpty Whether empty tags are collapsed in a single self closing tag
      * @param indent THe indentation string
      * @param attributeThreshold Number of attributes above which each attribute is rendered on a new line
      */
    def pretty[F[_]](collapseEmpty: Boolean = true,
                     indent: String = "  ",
                     attributeThreshold: Int = 3): Pipe[F, XmlEvent, String] =
      Renderer.pipe(collapseEmpty, indent, attributeThreshold)

  }

  val ncNameStart = CharRanges.fromRanges(
    ('A', 'Z'),
    ('_', '_'),
    ('a', 'z'),
    ('\u00C0', '\u00D6'),
    ('\u00D8', '\u00F6'),
    ('\u00F8', '\u02FF'),
    ('\u0370', '\u037D'),
    ('\u037F', '\u1FFF'),
    ('\u200C', '\u200D'),
    ('\u2070', '\u218F'),
    ('\u2C00', '\u2FEF'),
    ('\u3001', '\uD7FF'),
    ('\uF900', '\uFDCF'),
    ('\uFDF0', '\uFFFD')
  )

  val ncNameChar = ncNameStart.union(
    CharRanges
      .fromRanges(('-', '-'), ('.', '.'), ('0', '9'), ('\u00b7', '\u00b7'), ('\u0300', '\u036f'), ('\u203f', '\u2040')))

  def isNCNameStart(c: Char): Boolean =
    ncNameStart.contains(c)

  def isNCNameChar(c: Char): Boolean =
    ncNameChar.contains(c)

  def isXmlWhitespace(c: Char): Boolean =
    c == ' ' || c == '\t' || c == '\r' || c == '\n'

  /** XML event stream collectors. */
  object collector {

    /** Renders all events using the `Show` instance and build the result string. */
    object show extends Collector[XmlEvent] {
      type Out = String
      def newBuilder: Collector.Builder[XmlEvent, Out] =
        new Collector.Builder[XmlEvent, Out] {

          private val builder = new StringBuilder

          override def +=(c: Chunk[XmlEvent]): Unit =
            c.foreach(builder ++= _.show)

          override def result: Out =
            builder.result()

        }
    }

  }

  implicit class XmlInterpolators(val sc: StringContext) extends AnyVal {

    /** Creates a stream of XML token, dropping the comments. */
    def xml(args: Any*): Stream[Fallible, XmlEvent] =
      Stream.emit(sc.s(args: _*)).covary[Fallible].through(events(includeComments = false))

    /** Creates a stream of XML token, keeping the comments. */
    def rawxml(args: Any*): Stream[Fallible, XmlEvent] =
      Stream.emit(sc.s(args: _*)).covary[Fallible].through(events(includeComments = true))
  }

}
