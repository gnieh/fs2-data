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
  val encodeItems: Pipe[Pure, Token, CborItem] =
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
        if (bi.isValidByte)
          Chunk.singleton(CborItem.PositiveInt(ByteVector.fromByte(bi.toByte)))
        else if (bi.isValidShort)
          Chunk.singleton(CborItem.PositiveInt(ByteVector.fromShort(bi.toShort)))
        else if (bi.isValidInt)
          Chunk.singleton(CborItem.PositiveInt(ByteVector.fromInt(bi.toInt)))
        else if (bi.isValidLong)
          Chunk.singleton(CborItem.PositiveInt(ByteVector.fromLong(bi.toLong)))
        else
          Chunk(CborItem.Tag(Tags.PositiveBigNum), CborItem.ByteString(ByteVector(bi.toByteArray)))
      } else {
        val bi = (bd.toBigInt + 1).abs
        if (bi.isValidByte)
          Chunk.singleton(CborItem.NegativeInt(ByteVector.fromByte(bi.toByte)))
        else if (bi.isValidShort)
          Chunk.singleton(CborItem.NegativeInt(ByteVector.fromShort(bi.toShort)))
        else if (bi.isValidInt)
          Chunk.singleton(CborItem.NegativeInt(ByteVector.fromInt(bi.toInt)))
        else if (bi.isValidLong)
          Chunk.singleton(CborItem.PositiveInt(ByteVector.fromLong(bi.toLong)))
        else
          Chunk(CborItem.Tag(Tags.NegativeBigNum), CborItem.ByteString(ByteVector(bi.toByteArray)))
      }
    } else {
      if (bd.isDecimalFloat) {
        Chunk.singleton(CborItem.Float32(ByteVector.fromInt(JFloat.floatToIntBits(bd.toFloat))))
      } else if (bd.isDecimalDouble) {
        Chunk.singleton(CborItem.Float64(ByteVector.fromLong(JDouble.doubleToLongBits(bd.toDouble))))
      } else {
        val unscaled = BigDecimal(bd.underlying.unscaledValue())
        val scale = BigDecimal(-bd.underlying.scale())
        Chunk(CborItem.Tag(Tags.DecimalFraction), CborItem.StartArray(2)) ++ makeNumber(scale) ++ makeNumber(unscaled)
      }
    }
  }

}
