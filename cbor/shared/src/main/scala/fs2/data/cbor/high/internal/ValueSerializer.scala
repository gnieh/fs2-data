/*
 * Copyright 2023 Lucas Satabin
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
package high
package internal

import low.CborItem

import java.lang.{Float => JFloat, Double => JDouble}

import scodec.bits._

private[cbor] object ValueSerializer {

  private val one = BigInt(1)

  def toItems[F[_]]: Pipe[F, CborValue, CborItem] = {
    def go(chunk: Chunk[CborValue],
           idx: Int,
           rest: Stream[F, CborValue],
           acc: List[CborItem]): Pull[F, CborItem, Unit] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(acc.reverse)) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) => go(hd, 0, tl, Nil)
          case None           => Pull.done
        }
      } else {
        chunk(idx) match {
          case CborValue.Integer(i) =>
            // if representation is too big, use the tags specified in the RFC
            if (i >= 0) {
              val bytes = padIfNecessary(ByteVector(i.toByteArray))
              if (bytes.size <= 8)
                go(chunk, idx + 1, rest, CborItem.PositiveInt(bytes) :: acc)
              else
                go(chunk, idx + 1, rest, CborItem.ByteString(bytes) :: CborItem.Tag(Tags.PositiveBigNum) :: acc)
            } else {
              val bytes = padIfNecessary(ByteVector((-i - one).toByteArray))
              if (bytes.size <= 8)
                go(chunk, idx + 1, rest, CborItem.NegativeInt(bytes) :: acc)
              else
                go(chunk, idx + 1, rest, CborItem.ByteString(bytes) :: CborItem.Tag(Tags.NegativeBigNum) :: acc)
            }
          case CborValue.Array(values, indefinite) =>
            if (indefinite)
              go(Chunk.seq(values), 0, Stream.empty, CborItem.StartIndefiniteArray :: acc) >>
                go(chunk, idx + 1, rest, List(CborItem.Break))
            else
              go(Chunk.seq(values), 0, Stream.empty, CborItem.StartArray(values.size.toLong) :: acc) >>
                go(chunk, idx + 1, rest, Nil)
          case CborValue.Map(values, indefinite) =>
            if (indefinite)
              go(Chunk.iterable(values.flatMap(p => List(p._1, p._2))),
                 0,
                 Stream.empty,
                 CborItem.StartIndefiniteMap :: acc) >>
                go(chunk, idx + 1, rest, List(CborItem.Break))
            else
              go(Chunk.iterable(values.flatMap(p => List(p._1, p._2))),
                 0,
                 Stream.empty,
                 CborItem.StartMap(values.size.toLong) :: acc) >>
                go(chunk, idx + 1, rest, Nil)
          case CborValue.TextString(text) =>
            go(chunk, idx + 1, rest, CborItem.TextString(text) :: acc)
          case CborValue.ByteString(bytes) =>
            go(chunk, idx + 1, rest, CborItem.ByteString(bytes) :: acc)
          case CborValue.Tagged(tag, value) =>
            go(Chunk.singleton(value), 0, Stream.empty, CborItem.Tag(tag) :: acc) >>
              go(chunk, idx + 1, rest, Nil)
          case CborValue.Float32(value) =>
            go(chunk, idx + 1, rest, CborItem.Float32(ByteVector.fromInt(JFloat.floatToIntBits(value))) :: acc)
          case CborValue.Float64(value) =>
            go(chunk, idx + 1, rest, CborItem.Float64(ByteVector.fromLong(JDouble.doubleToLongBits(value))) :: acc)
          case CborValue.False =>
            go(chunk, idx + 1, rest, CborItem.False :: acc)
          case CborValue.True =>
            go(chunk, idx + 1, rest, CborItem.True :: acc)
          case CborValue.Null =>
            go(chunk, idx + 1, rest, CborItem.Null :: acc)
          case CborValue.Undefined =>
            go(chunk, idx + 1, rest, CborItem.Undefined :: acc)
          case CborValue.SimpleValue(value) =>
            go(chunk, idx + 1, rest, CborItem.SimpleValue(value) :: acc)
        }
      }

    go(Chunk.empty, 0, _, Nil).stream
  }

  private def padIfNecessary(vector: ByteVector): ByteVector = vector.size match {
    case 3         => vector.padLeft(4)
    case 5 | 6 | 7 => vector.padLeft(8)
    case _         => vector
  }

}
