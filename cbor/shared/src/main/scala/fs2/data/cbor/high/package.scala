/*
 * Copyright 2021 Lucas Satabin
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
package cbor

import low.CborItem
import high.internal.{ValueParser, ValueSerializer}

/** High-level representation and tools for CBOR data streams.
  *
  * The high-level representation is less powerful as the low-level one, as it builds
  * structured data. For instance it is not able to represent arrays of strings  whose size
  * is bigger than `Int.MaxValue`.
  *
  * The reprensentation is intended to be easier to work with if you need more structured
  * data and don't exceed the underlying limits.
  */
package object high {

  /** Parses the stream of bytes into high level AST. */
  def values[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Byte, CborValue] =
    _.through(low.items).through(parseValues)

  /** Parses the stream of low-level items into high level AST. */
  def parseValues[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, CborItem, CborValue] =
    ValueParser.pipe[F]

  /** Transforms a stream of CBOR values into a stream of low-level items.
    *
    * This encoder, uses some tags defined in [[fs2.data.cbor.Tags Tags]] to encode
    * some values (e.g. big numbers).
    */
  def toItems[F[_]]: Pipe[F, CborValue, CborItem] =
    ValueSerializer.toItems[F]

  /** Transforms a stream of CBOR values into the binary representations.
    *
    * This encoder, uses some tags defined in [[fs2.data.cbor.Tags Tags]] to encode
    * some values (e.g. big numbers).
    */
  def toBinary[F[_]]: Pipe[F, CborValue, Byte] =
    _.through(toItems).through(low.toNonValidatedBinary)

}
