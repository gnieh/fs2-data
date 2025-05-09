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
import cats.Monad
import scala.jdk.CollectionConverters._
import java.util.ArrayDeque

package object high extends DeserializerInstances with internal.PlatformDeserializerInstances {
  implicit val msgpackDeserializerMonad: Monad[MsgpackDeserializer] = MsgpackDeserializer.msgpackDeserializerMonad

  /** Converts a stream of [[fs2.data.msgpack.low.MsgpackItem]] into a stream of `A` via an implicit deserializer instance.
    */
  def fromItems[F[_], A](implicit F: RaiseThrowable[F], da: MsgpackDeserializer[A]): Pipe[F, MsgpackItem, A] = {
    import fs2.data.msgpack.high.internal.Helpers._

    stream =>
      def go(ctx: DeserializationContext[F], buffer: ArrayDeque[A]): Pull[F, A, Unit] = {
        if (ctx.idx >= ctx.chunk.size) {
          Pull.output(Chunk.from(buffer.asScala)) >> ctx.rest.pull.uncons.flatMap {
            case Some((hd, tl)) =>
              buffer.clear()
              go(DeserializationContext(hd, 0, tl), buffer)
            case None => Pull.done
          }
        } else {
          da.run(ctx).flatMap { case (item, ctx) =>
            buffer.add(item)
            go(ctx, buffer)
          }
        }
      }

      go(DeserializationContext(Chunk.empty, 0, stream), new ArrayDeque()).stream
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

  /** Summons a [[fs2.data.msgpack.high.MsgpackDeserializer]] instance for the desired type
    */
  def deserializer[A](implicit ev: MsgpackDeserializer[A]) = ev
}
