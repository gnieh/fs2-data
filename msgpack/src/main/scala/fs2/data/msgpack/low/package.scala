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
package msgpack

import low.internal.{ItemParser, ItemSerializer, ItemValidator}

/** A low-level representation of the MessagePack format.
  */
package object low {
  def items[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Byte, MsgpackItem] =
    ItemParser.pipe[F]

  /** Transforms a stream of [[MsgpackItem]]s into a stream of [[scala.Byte]]s.
    *
    * Will fail with an error if the stream is malformed.
    */
  def toBinary[F[_]: RaiseThrowable]: Pipe[F, MsgpackItem, Byte] =
    _.through(ItemValidator.pipe).through(ItemSerializer.pipe)

  def toNonValidatedBinary[F[_]: RaiseThrowable]: Pipe[F, MsgpackItem, Byte] =
    ItemSerializer.pipe

  def validate[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, MsgpackItem, MsgpackItem] =
    ItemValidator.pipe[F]
}
