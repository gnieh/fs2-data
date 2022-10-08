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

package fs2.data.cbor

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
object HalfFloat {

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
  def toFloat(raw: Short): Float = {
    val e = (raw >>> 10) & 31 // exponent
    val m = (raw & 1023) // mantissa
    val unsigned =
      if (e == 0) {
        // either zero or a subnormal number
        if (m != 0) math.pow(2F, -14).toFloat * (m / 1024F)
        else 0F
      } else if (e != 31) {
        // normal number
        math.pow(2F, e - 15D).toFloat * (1F + m / 1024F)
      } else if ((raw & 1023) != 0) {
        Float.NaN
      } else {
        Float.PositiveInfinity
      }
    if ((raw & 0x8000) == 0x8000)
      -unsigned
    else
      unsigned
  }

}
