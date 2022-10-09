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

import low._

import cats.syntax.all._
import cats.data.OptionT

import scodec.bits.Bases

import java.lang.{Float => JFloat, Double => JDouble}

/** Useful for debugging, generates a stream of diagnostic strings
  * representing the CBOR values in the input stream as defined in
  * [section 8 of RFC8949](https://www.rfc-editor.org/rfc/rfc8949.html#name-diagnostic-notation).
  */
object Diagnostic {

  private val minusOne = BigInt(-1)

  def apply[F[_]](s: Stream[F, CborItem])(implicit F: RaiseThrowable[F]): Stream[F, String] = {

    def strings(s: Stream[F, CborItem], sep: Char) = {
      def loop(s: Stream[F, CborItem],
               start: Boolean,
               acc: StringBuilder): Pull[F, String, Option[(String, Stream[F, CborItem])]] =
        s.pull.peek1.flatMap {
          case Some((item, s)) =>
            item match {
              case CborItem.Break =>
                if (start)
                  // empty sequence
                  Pull.pure((s"${sep}${sep}_", s.tail).some)
                else
                  // we already saw at least one element, close sequence
                  Pull.pure((acc.append(")").result(), s.tail).some)
              case _ =>
                if (start) acc.append("(_ ")
                else acc.append(", ")
                OptionT(value(s)).flatMapF { case (str, s) => loop(s, false, acc.append(str)) }.value
            }
          case None => Pull.pure(None)
        }

      loop(s, true, new StringBuilder)
    }

    def array(s: Stream[F, CborItem],
              size: Long,
              start: Boolean,
              acc: StringBuilder): Pull[F, String, Option[(String, Stream[F, CborItem])]] =
      if (size == 0L) {
        Pull.pure((acc.append("]").result(), s).some)
      } else {
        s.pull.peek1.flatMap {
          case Some((item, s)) =>
            item match {
              case CborItem.Break =>
                if (size < 0L)
                  Pull.pure((acc.append("]").result(), s.tail).some)
                else
                  Pull.raiseError(new CborParsingException("unexpected break"))
              case _ =>
                if (!start) acc.append(", ")
                OptionT(value(s)).flatMapF { case (str, s) =>
                  array(s, math.max(-1L, size - 1), false, acc.append(str))
                }.value
            }
          case None => Pull.pure(None)
        }
      }

    def map(s: Stream[F, CborItem],
            size: Long,
            start: Boolean,
            acc: StringBuilder): Pull[F, String, Option[(String, Stream[F, CborItem])]] =
      if (size == 0L) {
        Pull.pure((acc.append("}").result(), s).some)
      } else {
        s.pull.peek1.flatMap {
          case Some((item, s)) =>
            item match {
              case CborItem.Break =>
                if (size < 0L)
                  Pull.pure((acc.append("}").result(), s.tail).some)
                else
                  Pull.raiseError(new CborParsingException("unexpected break"))
              case _ =>
                if (!start) acc.append(", ")
                OptionT(value(s)).flatMap { case (k, s) =>
                  OptionT(value(s)).flatMapF { case (v, s) =>
                    map(s, math.max(-1L, size - 1), false, acc.append(k).append(": ").append(v))
                  }
                }.value
            }
          case None => Pull.pure(None)
        }
      }

    def value(s: Stream[F, CborItem]): Pull[F, String, Option[(String, Stream[F, CborItem])]] =
      s.pull.uncons1.flatMap {
        case Some((item, s)) =>
          item match {
            case CborItem.False              => Pull.pure(("false", s).some)
            case CborItem.True               => Pull.pure(("true", s).some)
            case CborItem.Null               => Pull.pure(("null", s).some)
            case CborItem.Undefined          => Pull.pure(("undefined", s).some)
            case CborItem.PositiveInt(bytes) => Pull.pure((BigInt(bytes.toHex, 16).toString(10), s).some)
            case CborItem.NegativeInt(bytes) =>
              Pull.pure(((minusOne - BigInt(bytes.toHex, 16)).toString(10), s).some)
            case CborItem.Float16(bytes) =>
              val f = HalfFloat.toFloat(bytes.toShort(signed = false))
              if (JFloat.isFinite(f))
                Pull.pure((s"${f}_1", s).some)
              else
                Pull.pure((f.toString(), s).some)
            case CborItem.Float32(bytes) =>
              val f = JFloat.intBitsToFloat(bytes.toInt(signed = false))
              if (JFloat.isFinite(f))
                Pull.pure((s"${f}_2", s).some)
              else
                Pull.pure((f.toString, s).some)
            case CborItem.Float64(bytes) =>
              val d = JDouble.longBitsToDouble(bytes.toLong(signed = false))
              if (JDouble.isFinite(d))
                Pull.pure((s"${d}_3", s).some)
              else
                Pull.pure((d.toString, s).some)
            case CborItem.SimpleValue(value)        => Pull.pure((s"simple(${value & 0xff})", s).some)
            case CborItem.TextString(str)           => Pull.pure((s""""$str"""", s).some)
            case CborItem.StartIndefiniteTextString => strings(s, '"')
            case CborItem.ByteString(bytes) =>
              Pull.pure((s"h'${bytes.toHex(Bases.Alphabets.HexUppercase)}'", s).some)
            case CborItem.StartIndefiniteByteString => strings(s, '\'')
            case CborItem.Tag(tag) =>
              OptionT(value(s)).map { case (str, s) => (s"$tag($str)", s) }.value
            case CborItem.StartArray(size)     => array(s, size, true, new StringBuilder().append("["))
            case CborItem.StartIndefiniteArray => array(s, -1, true, new StringBuilder().append("[_ "))
            case CborItem.StartMap(size)       => map(s, size, true, new StringBuilder().append("{"))
            case CborItem.StartIndefiniteMap   => map(s, -1, true, new StringBuilder().append("{_"))
            case CborItem.Break                => Pull.raiseError(new CborParsingException("unexpected break"))
          }
        case None =>
          Pull.pure(none)
      }

    Pull
      .loop((s: Stream[F, CborItem]) =>
        OptionT(value(s)).flatMapF { case (str, s) => Pull.output1(str).as(s.some) }.value)(s)
      .stream
  }

}
