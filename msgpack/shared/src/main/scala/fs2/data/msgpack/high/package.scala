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
import scala.collection.mutable

package object high {
  implicit val msgpackDecoderMonad: Monad[MsgpackDecoder] = MsgpackDecoder.msgpackDecoderMonad

  /** Converts a stream of [[fs2.data.msgpack.low.MsgpackItem]] into a stream of `A` via an implicit decoder instance.
    */
  def fromItems[F[_], A](implicit F: RaiseThrowable[F], da: MsgpackDecoder[A]): Pipe[F, MsgpackItem, A] = { stream =>
    import fs2.data.msgpack.high.internal.Helpers._

    def go(ctx: DecodingContext[F], buffer: mutable.ArrayDeque[A]): Pull[F, A, Unit] = {
      if (ctx.idx >= ctx.chunk.size) {
        Pull.output(Chunk.from(buffer)) >> ctx.rest.pull.uncons.flatMap {
          case Some((hd, tl)) => go(DecodingContext(hd, 0, tl), new mutable.ArrayDeque())
          case None           => Pull.done
        }
      } else {
        da.run(ctx).flatMap { case (item, ctx) =>
          go(ctx, buffer.addOne(item))
        }
      }
    }

    go(DecodingContext(Chunk.empty, 0, stream), new mutable.ArrayDeque()).stream
  }

  /** Converts a stream of [[fs2.data.msgpack.low.MsgpackItem]] into a stream of `A` via an explicit decoder instance.
    */
  @inline def fromItems[F[_], A](da: MsgpackDecoder[A])(implicit F: RaiseThrowable[F]): Pipe[F, MsgpackItem, A] =
    fromItems(F, da)

  /** Decodes a stream of bytes into a stream of `A`s via an implicit decoder instance. 
    */
  def decode[F[_]: RaiseThrowable, A: MsgpackDecoder]: Pipe[F, Byte, A] =
    _.through(low.items[F]).through(fromItems[F, A])

  /** Decodes a stream of bytes into a stream of `A`s via an explicit decoder instance. 
    */
  @inline def decode[F[_], A](da: MsgpackDecoder[A])(implicit F: RaiseThrowable[F]): Pipe[F, Byte, A] =
    decode(F, da)

  /** Summons a [[fs2.data.msgpack.high.MsgpackDecoder]] instance for the desired type
    */
  def decoder[A](implicit ev: MsgpackDecoder[A]) = ev

  object static extends StaticDecoderInstances with internal.PlatformStaticDecoderInstances

  object dynamic extends DynamicDecoderInstances {

    /** Converts a stream of bytes into a stream [[MsgpackItem]]s into a stream of [[MsgpackValue$]]s. Alias for [[fromItems]][F, [[MsgpackValue]]].
      */
    @inline def valuesFromItems[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, MsgpackItem, MsgpackValue] =
      fromItems[F, MsgpackValue]

    /** Decodes a stream of bytes into a stream of [[MsgpackValue]]s. Alias for [[decoder]][F, [[MsgpackValue$]]]
      */
    @inline def values[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Byte, MsgpackValue] = decode[F, MsgpackValue]
  }
}
