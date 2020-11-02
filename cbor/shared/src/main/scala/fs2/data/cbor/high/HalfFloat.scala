/*
 * Copyright 2020 Lucas Satabin
 * Adapted from https://gist.github.com/non/29f8d66036afca402f96
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

package fs2.data.cbor.high

import java.lang.{Float => JFloat}
import scodec.bits.BitVector

/** HalfFloat represents 16-bit floating-point values.
  *
  * This type does not actually support arithmetic directly. The
  * expected use case is to convert to Float to perform any actual
  * arithmetic, then convert back to a HalfFloat if needed.
  *
  * Binary representation:
  *
  *     sign (1 bit)
  *     |
  *     | exponent (5 bits)
  *     | |
  *     | |     mantissa (10 bits)
  *     | |     |
  *     x xxxxx xxxxxxxxxx
  *
  * Value interpretation (in order of precedence, with _ wild):
  *
  *     0 00000 0000000000  (positive) zero
  *     1 00000 0000000000  negative zero
  *     _ 00000 __________  subnormal number
  *     _ 11111 0000000000  +/- infinity
  *     _ 11111 __________  not-a-number
  *     _ _____ __________  normal number
  *
  * For non-zero exponents, the mantissa has an implied leading 1 bit,
  * so 10 bits of data provide 11 bits of precision for normal numbers.
  */
class HalfFloat(val raw: Short) extends AnyVal { lhs =>

  def isNaN: Boolean = (raw & 0x7fff) > 0x7c00
  def nonNaN: Boolean = (raw & 0x7fff) <= 0x7c00

  /** Returns if this is a zero value (positive or negative).
    */
  def isZero: Boolean = (raw & 0x7fff) == 0

  def nonZero: Boolean = (raw & 0x7fff) != 0

  def isPositiveZero: Boolean = raw == -0x8000
  def isNegativeZero: Boolean = raw == 0

  def isInfinite: Boolean = (raw & 0x7fff) == 0x7c00
  def isPositiveInfinity: Boolean = raw == 0x7c00
  def isNegativeInfinity: Boolean = raw == 0xfc00

  /** Whether this HalfFloat value is finite or not.
    *
    * For the purposes of this method, infinities and NaNs are
    * considered non-finite. For those values it returns false and for
    * all other values it returns true.
    */
  def isFinite: Boolean = (raw & 0x7c00) != 0x7c00

  /** Return the sign of a HalfFloat value as a Float.
    *
    * There are five possible return values:
    *
    *  * NaN: the value is HalfFloat.NaN (and has no sign)
    *  * -1F: the value is a non-zero negative number
    *  * -0F: the value is HalfFloat.NegativeZero
    *  *  0F: the value is HalfFloat.Zero
    *  *  1F: the value is a non-zero positive number
    *
    * PositiveInfinity and NegativeInfinity return their expected
    * signs.
    */
  def signum: Float =
    if (raw == -0x8000) 0f
    else if (raw == 0) -0f
    else if ((raw & 0x7fff) > 0x7c00) Float.NaN
    else ((raw >>> 14) & 2) - 1f

  /** Reverse the sign of this HalfFloat value.
    *
    * This just involves toggling the sign bit with XOR.
    *
    * -HalfFloat.NaN has no meaningful effect.
    * -HalfFloat.Zero returns HalfFloat.NegativeZero.
    */
  def unary_- : HalfFloat =
    new HalfFloat((raw ^ 0x8000).toShort)

  def +(rhs: HalfFloat): HalfFloat =
    HalfFloat.fromFloat(lhs.toFloat + rhs.toFloat)
  def -(rhs: HalfFloat): HalfFloat =
    HalfFloat.fromFloat(lhs.toFloat - rhs.toFloat)
  def *(rhs: HalfFloat): HalfFloat =
    HalfFloat.fromFloat(lhs.toFloat * rhs.toFloat)
  def /(rhs: HalfFloat): HalfFloat =
    HalfFloat.fromFloat(lhs.toFloat / rhs.toFloat)
  def **(rhs: Int): HalfFloat =
    HalfFloat.fromFloat(math.pow(lhs.toFloat, rhs).toFloat)

  def <(rhs: HalfFloat): Boolean = {
    if (lhs.raw == rhs.raw || lhs.isNaN || rhs.isNaN) return false
    if (lhs.isZero && rhs.isZero) return false
    val ls = (lhs.raw >>> 15) & 1
    val rs = (rhs.raw >>> 15) & 1
    if (ls < rs) return true
    if (ls > rs) return false
    val le = (lhs.raw >>> 10) & 31
    val re = (rhs.raw >>> 10) & 31
    if (le < re) return ls == 1
    if (le > re) return ls == 0
    val lm = lhs.raw & 1023
    val rm = rhs.raw & 1023
    if (ls == 1) lm < rm else rm < lm
  }

  def <=(rhs: HalfFloat): Boolean = {
    if (lhs.isNaN || rhs.isNaN) return false
    if (lhs.isZero && rhs.isZero) return true
    val ls = (lhs.raw >>> 15) & 1
    val rs = (rhs.raw >>> 15) & 1
    if (ls < rs) return true
    if (ls > rs) return false
    val le = (lhs.raw >>> 10) & 31
    val re = (rhs.raw >>> 10) & 31
    if (le < re) return ls == 1
    if (le > re) return ls == 0
    val lm = lhs.raw & 1023
    val rm = rhs.raw & 1023
    if (ls == 1) lm <= rm else rm <= lm
  }

  def >(rhs: HalfFloat): Boolean =
    !(lhs.isNaN || rhs.isNaN || lhs <= rhs)

  def >=(rhs: HalfFloat): Boolean =
    !(lhs.isNaN || rhs.isNaN || lhs < rhs)

  def ==(rhs: HalfFloat): Boolean =
    if (lhs.isNaN || rhs.isNaN) false
    else if (lhs.isZero && rhs.isZero) true
    else lhs.raw == rhs.raw

  /** Convert this HalfFloat value to the nearest Float.
    *
    * Non-finite values and zero values will be mapped to the
    * corresponding Float value.
    *
    * All other finite values will be handled depending on whether they
    * are normal or subnormal. The relevant formulas are:
    *
    * * normal:    (sign*2-1) * 2^(exponent-15) * (1 + mantissa/1024)
    * * subnormal: (sign*2-1) * 2^-14 * (mantissa/1024)
    *
    * Given any (x: HalfFloat), HalfFloat.fromFloat(x.toFloat) = x
    *
    * The reverse is not necessarily true, since there are many Float
    * values which are not precisely representable as HalfFloat values.
    */
  def toFloat: Float = {
    val s = (raw >>> 14) & 2 // sign*2
    val e = (raw >>> 10) & 31 // exponent
    val m = (raw & 1023) // mantissa
    if (e == 0) {
      // either zero or a subnormal number
      if (m != 0) (s - 1f) * math.pow(2f, -14).toFloat * (m / 1024f)
      else if (s == 0) -0f
      else 0f
    } else if (e != 31) {
      // normal number
      (s - 1f) * math.pow(2f, e - 15).toFloat * (1f + m / 1024f)
    } else if ((raw & 1023) != 0) {
      Float.NaN
    } else if (raw < 0) {
      Float.PositiveInfinity
    } else {
      Float.NegativeInfinity
    }
  }

  /** String representation of this HalfFloat value.
    */
  override def toString: String =
    toFloat.toString
}

object HalfFloat {

  // interesting HalfFloat constants
  // with the exception of NaN, values go from smallest to largest
  val NaN = new HalfFloat(0x7c01.toShort)
  val NegativeInfinity = new HalfFloat(0x7c00.toShort)
  val MinValue = new HalfFloat(0x7bff.toShort)
  val MinusOne = new HalfFloat(0x3c00.toShort)
  val MaxNegativeNormal = new HalfFloat(0x0400.toShort)
  val MaxNegative = new HalfFloat(0x0001.toShort)
  val NegativeZero = new HalfFloat(0x0000.toShort)
  val Zero = new HalfFloat(0x8000.toShort)
  val MinPositive = new HalfFloat(0x8001.toShort)
  val MinPositiveNormal = new HalfFloat(0x8400.toShort)
  val One = new HalfFloat(0xbc00.toShort)
  val MaxValue = new HalfFloat(0xfbff.toShort)
  val PositiveInfinity = new HalfFloat(0xfc00.toShort)

  def fromBits(bits: BitVector): HalfFloat =
    new HalfFloat(bits.toShort(signed = false))

  /** Create a HalfFloat value from a Float.
    *
    * This value is guaranteed to be the closest possible HalfFloat
    * value. However, because there are many more possible Float
    * values, rounding will occur, and very large or very small values
    * will end up as infinities.
    *
    * Given any (x: HalfFloat), HalfFloat.fromFloat(x.toFloat) = x
    *
    * The reverse is not necessarily true, since there are many Float
    * values which are not precisely representable as HalfFloat values.
    */
  def fromFloat(n: Float): HalfFloat = {

    // given e, an exponent in [-14, 15], and x, a double value,
    // return the HalfFloat value that is closest to (x * 2^e).
    def createFinite(e: Int, x: Float): HalfFloat = {
      def createNormal(e: Int, m: Int): HalfFloat =
        new HalfFloat((((e + 15) << 10) | m | 0x8000).toShort)
      val m = math.round((x - 1f) * 1024).toInt
      if (e == -14 && m == 0) {
        HalfFloat.MinPositiveNormal
      } else if (e == -14) {
        if (m == 0) HalfFloat.Zero
        else if (x >= 1f) createNormal(e, m)
        else new HalfFloat((0x8000 | math.round(x * 1024)).toShort)
      } else if (e == 15) {
        if (m < 1024) createNormal(e, m)
        else HalfFloat.PositiveInfinity
      } else {
        createNormal(e, m)
      }
    }

    if (JFloat.isNaN(n)) HalfFloat.NaN
    else if (n == Float.PositiveInfinity) HalfFloat.PositiveInfinity
    else if (n == Float.NegativeInfinity) HalfFloat.NegativeInfinity
    else if (JFloat.compare(n, -0f) == 0) HalfFloat.NegativeZero
    else if (n == 0f) HalfFloat.Zero
    else if (n < 0f) -fromFloat(-n)
    else if (1f <= n && n < 2f) createFinite(0, n)
    else {
      var e = 0
      var x = n
      if (n < 1f) {
        while (x < 1f && e > -14) { x *= 2f; e -= 1 }
      } else {
        while (x >= 2f && e < 15) { x *= 0.5f; e += 1 }
      }
      createFinite(e, x)
    }
  }
}
