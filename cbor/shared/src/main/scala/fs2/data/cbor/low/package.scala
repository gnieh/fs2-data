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
package cbor

import low.internal._

import scodec.bits._

import java.nio.charset.StandardCharsets
import java.lang.{Long => JLong}

/** Low-level representation and tools for CBOR data streams.
  *
  * The low-level representation has two main goals:
  *  - it is a flat representation of the input stream, which allows for
  *    expressing collections that exceed the max size of `Int.MaxValue`
  *    - it doesn't interpret numbers, keeping their raw representation in
  *      memory.
  *
  *   This representation is useful when dealing with streams that may contain
  *   big collections or when it is not necessary to build an AST, as it is more
  *   efficient than the high-level one.
  *
  *   The data model follows closely the structure described in the RFC.
  */
package object low {

  /** Parses the input byte stream into a sequence of low-level CBOR items.
    * This allows for parsing arbitrary long and deep CBOR data. No AST
    * is built.
    */
  def items[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Byte, CborItem] =
    ItemParser.pipe[F]

  /** Validates the stream of CBOR items, emitting them unchanged.
    *
    * The resulting stream is failed as soon as a problem is encountered.
    */
  def validate[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, CborItem, CborItem] =
    ItemValidator.pipe[F]

  /** Transforms a stream of CBOR items into the binary representation.
    *
    * The resulting stream fails as soon as a problem is encounter.
    */
  def toBinary[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, CborItem, Byte] =
    _.through(validate).through(toNonValidatedBinary)

  /** Transforms a stream of CBOR items into the binary representation.
    *
    * This produces a valid byte stream if the input item stream is well-formed,
    * otherwise result is uncertain and will eventually produce an invalid byte
    * sequence.
    *
    * Use this pipe if you are sure the stream is well-formed or if it does not
    * matter to produce ill-formed streams.
    *
    * Since no validation is performed, this pipe is more efficient and allows for
    * better throughput.
    */
  def toNonValidatedBinary[F[_]]: Pipe[F, CborItem, Byte] =
    _.mapChunks {
      _.flatMap {
        case CborItem.PositiveInt(bytes) =>
          // Major type 0
          if (bytes.size == 1) {
            if ((bytes.head & 0xff) < 24)
              Chunk.byteVector(bytes)
            else
              Chunk.byteVector(MajorType.Zero1 ++ bytes)
          } else if (bytes.size == 2) {
            Chunk.byteVector(MajorType.Zero2 ++ bytes)
          } else if (bytes.size == 4) {
            Chunk.byteVector(MajorType.Zero4 ++ bytes)
          } else {
            Chunk.byteVector(MajorType.Zero8 ++ bytes)
          }
        case CborItem.NegativeInt(bytes) =>
          // Major type 1
          if (bytes.size == 1) {
            if ((bytes.head & 0xff) < 24)
              Chunk.byteVector(bytes | MajorType.One0)
            else
              Chunk.byteVector(MajorType.One1 ++ bytes)
          } else if (bytes.size == 2) {
            Chunk.byteVector(MajorType.One2 ++ bytes)
          } else if (bytes.size == 4) {
            Chunk.byteVector(MajorType.One4 ++ bytes)
          } else {
            Chunk.byteVector(MajorType.One8 ++ bytes)
          }
        case CborItem.ByteString(bytes) =>
          // Major type 2
          Chunk.concat(
            List(encodeLong(bytes.size, MajorType.Two0, MajorType.Two1, MajorType.Two2, MajorType.Two4, MajorType.Two8),
                 Chunk.byteVector(bytes)))
        case CborItem.StartIndefiniteByteString =>
          // Major type 2 (indefinite variant)
          Chunk.byteVector(MajorType.TwoIndefinite)
        case CborItem.TextString(str) =>
          // Major type 3
          val encoded = ByteVector(str.getBytes(StandardCharsets.UTF_8))
          Chunk.concat(
            List(encodeLong(encoded.size,
                            MajorType.Three0,
                            MajorType.Three1,
                            MajorType.Three2,
                            MajorType.Three4,
                            MajorType.Three8),
                 Chunk.byteVector(encoded)))
        case CborItem.StartIndefiniteTextString =>
          // Major type 3 (indefinite variant)
          Chunk.byteVector(MajorType.ThreeIndefinite)
        case CborItem.StartArray(size) =>
          // Major type 4
          encodeLong(size, MajorType.Four0, MajorType.Four1, MajorType.Four2, MajorType.Four4, MajorType.Four8)
        case CborItem.StartIndefiniteArray =>
          // Major type 4 (indefinite variant)
          Chunk.byteVector(MajorType.FourIndefinite)
        case CborItem.StartMap(size) =>
          // Major type 5
          encodeLong(size, MajorType.Five0, MajorType.Five1, MajorType.Five2, MajorType.Five4, MajorType.Five8)
        case CborItem.StartIndefiniteMap =>
          // Major type 5 (indefinite variant)
          Chunk.byteVector(MajorType.FiveIndefinite)
        case CborItem.Tag(value) =>
          // Major type 6
          encodeLong(value, MajorType.Six0, MajorType.Six1, MajorType.Six2, MajorType.Six4, MajorType.Six8)
        case CborItem.SimpleValue(simple) =>
          // Major type 7
          val bytes = ByteVector(simple)
          if ((simple & 0xff) < 24)
            Chunk.byteVector(bytes | MajorType.Seven0)
          else
            Chunk.byteVector(MajorType.Seven1 ++ bytes)
        case CborItem.Float16(bytes) =>
          // Major type 7
          Chunk.byteVector(MajorType.Seven2 ++ bytes)
        case CborItem.Float32(bytes) =>
          // Major type 7
          Chunk.byteVector(MajorType.Seven4 ++ bytes)
        case CborItem.Float64(bytes) =>
          // Major type 7
          Chunk.byteVector(MajorType.Seven8 ++ bytes)
        case CborItem.Break =>
          // Major type 7
          Chunk.byteVector(MajorType.SevenBreak)
        case CborItem.False =>
          // Major type 7
          Chunk.byteVector(MajorType.SevenFalse)
        case CborItem.True =>
          // Major type 7
          Chunk.byteVector(MajorType.SevenTrue)
        case CborItem.Null =>
          // Major type 7
          Chunk.byteVector(MajorType.SevenNull)
        case CborItem.Undefined =>
          // Major type 7
          Chunk.byteVector(MajorType.SevenUndefined)
      }
    }

  private def encodeLong(l: Long,
                         zero: ByteVector,
                         one: ByteVector,
                         two: ByteVector,
                         four: ByteVector,
                         eight: ByteVector) =
    if (JLong.compareUnsigned(l, 23) <= 0) {
      Chunk.byteVector(ByteVector.fromByte((l & 0x1f).toByte) | zero)
    } else if (JLong.compareUnsigned(l, Byte.MaxValue) <= 0) {
      Chunk.byteVector(one ++ ByteVector.fromByte((l & 0xff).toByte))
    } else if (JLong.compareUnsigned(l, Short.MaxValue) <= 0) {
      Chunk.byteVector(two ++ ByteVector.fromShort((l & 0xffff).toShort))
    } else if (JLong.compareUnsigned(l, Int.MaxValue) <= 0) {
      Chunk.byteVector(four ++ ByteVector.fromInt((l & 0xffffffff).toInt))
    } else {
      Chunk.byteVector(eight ++ ByteVector.fromLong(l))
    }

}
