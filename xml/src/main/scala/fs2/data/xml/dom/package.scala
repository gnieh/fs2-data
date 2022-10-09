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

package object dom {

  /** Transforms a stream of XML events into a stream of XML document trees.
    */
  def documents[F[_], Node](implicit F: RaiseThrowable[F], builder: DocumentBuilder[Node]): Pipe[F, XmlEvent, Node] =
    new TreeParser[F, Node].pipe(builder) // explicit implicit for bincompat overloading resolution

  /** Transforms a stream of XML events into a stream of XML elements trees.
    *
    * This pipe will fail if the top-level events do not describe XML elements.
    */
  def elements[F[_], Elt](implicit F: RaiseThrowable[F], builder: ElementBuilder.Aux[Elt]): Pipe[F, XmlEvent, Elt] =
    new TreeParser[F, Elt].elements

  /** Transforms a stream of XML nodes into a stream of XML events.
    */
  def eventify[F[_], Node](implicit eventifier: DocumentEventifier[Node]): Pipe[F, Node, XmlEvent] =
    _.flatMap(node => eventifier.eventify(node))

}
