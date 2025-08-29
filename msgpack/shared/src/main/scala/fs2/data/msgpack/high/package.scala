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

import fs2.data.msgpack.low.MsgpackItem
import fs2.data.msgpack.high.DeserializationResult._
import cats.Monad
import scala.jdk.CollectionConverters._

package object high extends DeserializerInstances with internal.PlatformDeserializerInstances {
  implicit val msgpackDeserializerMonad: Monad[MsgpackDeserializer] = MsgpackDeserializer.msgpackDeserializerMonad

  def fromItems[F[_], A](implicit F: RaiseThrowable[F], da: MsgpackDeserializer[A]): Pipe[F, MsgpackItem, A] = {
    stream =>
      val buffer = new java.util.ArrayDeque[A]()

      def go(current: Chunk[MsgpackItem],
             rest: Stream[F, MsgpackItem],
             needsMoreItems: Boolean,
             itemCountHint: Option[Long]): Pull[F, A, Unit] = {
        if (needsMoreItems) {
          val uncons = itemCountHint match {
            case Some(count) if count <= Int.MaxValue =>
              rest.pull.unconsMin(count.toInt) // this makes sense only for one-dimensional arrays/maps
            case _ => rest.pull.uncons
          }

          uncons.flatMap {
            case Some((head, tail)) => go(current ++ head, tail, false, None)
            case None               => Pull.raiseError(new MsgpackUnexpectedEndOfStreamException())
          }
        } else if (current.isEmpty) {
          Pull.output(Chunk.from(buffer.asScala)) >> rest.pull.uncons.flatMap {
            case Some((head, tail)) =>
              buffer.clear()
              go(current ++ head, tail, false, None)
            case None => Pull.done
          }
        } else {
          da.deserialize(current) match {
            case Err(msg)             => Pull.raiseError(new MsgpackDeserializerException(msg))
            case NeedsMoreItems(hint) => go(current, rest, true, hint)
            case Ok(value, reminder) =>
              buffer.add(value)
              go(reminder, rest, false, None)
          }
        }
      }

      go(Chunk.empty, stream, false, None).stream
  }

  /** Converts a stream of [[fs2.data.msgpack.low.MsgpackItem]] into a stream of `A` via an explicit deserializer instance.
    */
  @inline def fromItems[F[_], A](da: MsgpackDeserializer[A])(implicit F: RaiseThrowable[F]): Pipe[F, MsgpackItem, A] =
    fromItems(F, da)

  /** Decodes a stream of bytes into a stream of `A`s via an implicit deserializer instance.
    */
  def deserialize[F[_]: RaiseThrowable, A: MsgpackDeserializer]: Pipe[F, Byte, A] =
    _.through(low.items[F]).through(fromItems[F, A])

  /** Decodes a stream of bytes into a stream of `A`s via an explicit deserializer instance.
    */
  @inline def deserialize[F[_], A](da: MsgpackDeserializer[A])(implicit F: RaiseThrowable[F]): Pipe[F, Byte, A] =
    deserialize(F, da)
}
