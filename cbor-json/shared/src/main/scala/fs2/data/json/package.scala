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
package json

import fs2.data.cbor.low.CborItem
import fs2.data.cbor.Tags

import scodec.bits.ByteVector

import java.lang.{Double => JDouble, Float => JFloat}

package object cbor {

  /** Purely streaming encoding pipe. No type hint is given,
    * which results in all collection being of indefinite size.
    *
    * For numbers (integers and floating) it selects the smallest
    * available representation that represents it exactly.
    * However, this will never emit half floats.
    */
  def encodeItems[F[_]]: Pipe[F, Token, CborItem] =
    _.mapChunks { chunk =>
      chunk.flatMap {
        case Token.FalseValue       => Chunk.singleton(CborItem.False)
        case Token.TrueValue        => Chunk.singleton(CborItem.True)
        case Token.NullValue        => Chunk.singleton(CborItem.Null)
        case Token.StringValue(s)   => Chunk.singleton(CborItem.TextString(s))
        case Token.NumberValue(raw) => makeNumber(raw)
        case Token.StartArray       => Chunk.singleton(CborItem.StartIndefiniteArray)
        case Token.EndArray         => Chunk.singleton(CborItem.Break)
        case Token.StartObject      => Chunk.singleton(CborItem.StartIndefiniteMap)
        case Token.Key(k)           => Chunk.singleton(CborItem.TextString(k))
        case Token.EndObject        => Chunk.singleton(CborItem.Break)
      }
    }

  private def makeNumber(raw: String): Chunk[CborItem] =
    makeNumber(BigDecimal(raw))

  private def makeNumber(bd: BigDecimal): Chunk[CborItem] = {
    if (bd.isWhole) {
      if (bd >= 0) {
        val bi = bd.toBigInt
        if (bi.isValidLong)
          Chunk.singleton(CborItem.PositiveInt(ByteVector(bi.toByteArray)))
        else
          Chunk(CborItem.Tag(Tags.PositiveBigNum), CborItem.ByteString(ByteVector(bi.toByteArray).dropWhile(_ == 0x00)))
      } else {
        val bi = (bd.toBigInt + 1).abs
        if (bi.isValidLong)
          Chunk.singleton(CborItem.NegativeInt(ByteVector(bi.toByteArray)))
        else
          Chunk(CborItem.Tag(Tags.NegativeBigNum), CborItem.ByteString(ByteVector(bi.toByteArray).dropWhile(_ == 0x00)))
      }
    } else {
      if (bd.isBinaryFloat) {
        Chunk.singleton(CborItem.Float32(ByteVector.fromInt(JFloat.floatToIntBits(bd.toFloat))))
      } else if (bd.isBinaryDouble) {
        Chunk.singleton(CborItem.Float64(ByteVector.fromLong(JDouble.doubleToLongBits(bd.toDouble))))
      } else {
        val unscaled = BigDecimal(bd.underlying.unscaledValue())
        val scale = BigDecimal(-bd.underlying.scale())
        Chunk(CborItem.Tag(Tags.DecimalFraction), CborItem.StartArray(2)) ++ makeNumber(scale) ++ makeNumber(unscaled)
      }
    }
  }

}
