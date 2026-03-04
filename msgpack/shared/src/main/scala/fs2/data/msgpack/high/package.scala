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
import cats._
import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuilder

package object high extends DeserializerInstances with SerializerInstances {
  implicit class MsgpackSerializerSyntax[A](x: A)(implicit sa: MsgpackSerializer[A]) {
    @inline def serialize = sa(x)
  }

  type SerializationResult = Either[String, Array[MsgpackItem]]

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
            case Ok(value, reminder)  =>
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

  /** Deserializes a stream of bytes into a stream of `A`s via an implicit deserializer instance.
    */
  def deserialize[F[_]: RaiseThrowable, A: MsgpackDeserializer]: Pipe[F, Byte, A] =
    _.through(low.fromBinary[F]).through(fromItems[F, A])

  /** Deserializes a stream of bytes into a stream of `A`s via an explicit deserializer instance.
    */
  @inline def deserialize[F[_], A](da: MsgpackDeserializer[A])(implicit F: RaiseThrowable[F]): Pipe[F, Byte, A] =
    deserialize(F, da)

  /** Converts a stream of `A` into a stream of [[fs2.data.msgpack.low.MsgpackItem$ MsgpackItems]] via an explicit
    * [[fs2.data.msgpack.high.MsgpackSerializer$ MsgpackSerializer]] instance.
    */
  def toItems[F[_]: RaiseThrowable, A](implicit sa: MsgpackSerializer[A]): Pipe[F, A, MsgpackItem] = { stream =>
    @scala.annotation.tailrec
    def processChunk(chunk: Chunk[A],
                     idx: Int,
                     chunkLength: Int /*caching it turns out to improve performance in a non negligible way */,
                     acc: ArrayBuilder[MsgpackItem]): Pull[F, MsgpackItem, Unit] =
      if (idx >= chunkLength) {
        // done processing this chunk
        Pull.output(Chunk.array(acc.result()))
      } else {
        val item = chunk(idx)
        sa(item) match {
          case Left(e) => Pull.output(Chunk.array(acc.result())) >> Pull.raiseError(new MsgpackSerializerException(e))
          case Right(items) => processChunk(chunk, idx + 1, chunkLength, acc ++= items)
        }
      }

    val builder = ArrayBuilder.make[MsgpackItem]

    def go(rest: Stream[F, A]): Pull[F, MsgpackItem, Unit] =
      rest.pull.uncons.flatMap {
        case Some((headChunk, tail)) =>
          builder.clear()
          processChunk(headChunk, 0, headChunk.size, builder) >> go(tail)
        case None => Pull.done
      }

    go(stream).stream
  }

  /** Converts a stream of `A` into a stream of [[fs2.data.msgpack.low.MsgpackItem$ MsgpackItems]] via an implicit
    * [[fs2.data.msgpack.high.MsgpackSerializer$ MsgpackSerializer]] instance.
    */
  @inline def toItems[F[_], A](sa: MsgpackSerializer[A])(implicit F: RaiseThrowable[F]): Pipe[F, A, MsgpackItem] =
    toItems(F, sa)

  /** Serializes a stream of `A`s into a stream of bytes via an implicit deserializer instance.
    */
  @inline def serialize[F[_]: RaiseThrowable, A: MsgpackSerializer]: Pipe[F, A, Byte] =
    _.through(high.toItems[F, A]).through(low.toNonValidatedBinary[F])

  /** Serializes a stream of `A`s into a stream of bytes via an implicit deserializer instance.
    */
  @inline def serialize[F[_], A](sa: MsgpackSerializer[A])(implicit F: RaiseThrowable[F]): Pipe[F, A, Byte] =
    serialize(F, sa)
}
