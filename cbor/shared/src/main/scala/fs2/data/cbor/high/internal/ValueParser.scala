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
package high
package internal

import low.CborItem

import scala.collection.mutable

import java.lang.{Float => JFloat, Double => JDouble, Long => JLong}
import scodec.bits.ByteVector

object ValueParser {

  private type Result[F[_], T] = (Chunk[CborItem], Int, Stream[F, CborItem], List[CborValue], T)

  private def raise[F[_]](e: CborParsingException, chunkAcc: List[CborValue])(implicit
      F: RaiseThrowable[F]): Pull[F, CborValue, Nothing] =
    Pull.output(Chunk.seq(chunkAcc.reverse)) >> Pull.raiseError(e)

  private def parseArray[F[_]](chunk: Chunk[CborItem],
                               idx: Int,
                               rest: Stream[F, CborItem],
                               size: Long,
                               acc: mutable.ListBuffer[CborValue],
                               chunkAcc: List[CborValue])(implicit
      F: RaiseThrowable[F]): Pull[F, CborValue, Result[F, CborValue]] =
    if (size == 0L) {
      Pull.pure((chunk, idx, rest, chunkAcc, CborValue.Array(acc.result(), false)))
    } else {
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) => parseArray(hd, 0, tl, size, acc, Nil)
          case None           => Pull.raiseError(new CborParsingException("unexpected end of input"))
        }
      } else {
        if (JLong.compareUnsigned(size, Int.MaxValue.toLong) > 0) {
          raise(
            new CborParsingException(
              s"array size is limited to max int (${Int.MaxValue}) elements but got ${JLong.toUnsignedString(size)}"),
            chunkAcc)
        } else {
          parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, value) =>
            acc.append(value)
            parseArray(chunk, idx, rest, size - 1, acc, chunkAcc)
          }
        }
      }
    }

  private def parseIndefiniteArray[F[_]](chunk: Chunk[CborItem],
                                         idx: Int,
                                         rest: Stream[F, CborItem],
                                         acc: mutable.ListBuffer[CborValue],
                                         chunkAcc: List[CborValue])(implicit
      F: RaiseThrowable[F]): Pull[F, CborValue, Result[F, CborValue]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) => parseIndefiniteArray(hd, 0, tl, acc, Nil)
        case None           => Pull.raiseError(new CborParsingException("unexpected end of input"))
      }
    } else {
      chunk(idx) match {
        case CborItem.Break =>
          Pull.pure((chunk, idx + 1, rest, chunkAcc, CborValue.Array(acc.result(), true)))
        case _ =>
          if (acc.size == Int.MaxValue) {
            raise(new CborParsingException(s"array size is limited to max int (${Int.MaxValue}) elements"), chunkAcc)
          } else {
            parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, value) =>
              acc.append(value)
              parseIndefiniteArray(chunk, idx, rest, acc, chunkAcc)
            }
          }
      }
    }

  private def parseMap[F[_]](chunk: Chunk[CborItem],
                             idx: Int,
                             rest: Stream[F, CborItem],
                             size: Long,
                             acc: mutable.Map[CborValue, CborValue],
                             chunkAcc: List[CborValue])(implicit
      F: RaiseThrowable[F]): Pull[F, CborValue, Result[F, CborValue]] =
    if (size == 0L) {
      Pull.pure((chunk, idx, rest, chunkAcc, CborValue.Map(acc.result(), false)))
    } else {
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) => parseMap(hd, 0, tl, size, acc, Nil)
          case None           => Pull.raiseError(new CborParsingException("unexpected end of input"))
        }
      } else {
        if (JLong.compareUnsigned(size, Int.MaxValue.toLong) > 0) {
          raise(new CborParsingException(
                  s"map size is limited to max int (${Int.MaxValue}) elements but got ${JLong.toUnsignedString(size)}"),
                chunkAcc)
        } else {
          parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, key) =>
            parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, value) =>
              acc.update(key, value)
              parseMap(chunk, idx, rest, size - 1, acc, chunkAcc)
            }
          }
        }
      }
    }

  private def parseIndefiniteMap[F[_]](chunk: Chunk[CborItem],
                                       idx: Int,
                                       rest: Stream[F, CborItem],
                                       acc: mutable.Map[CborValue, CborValue],
                                       chunkAcc: List[CborValue])(implicit
      F: RaiseThrowable[F]): Pull[F, CborValue, Result[F, CborValue]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) => parseIndefiniteMap(hd, 0, tl, acc, Nil)
        case None           => Pull.raiseError(new CborParsingException("unexpected end of input"))
      }
    } else {
      chunk(idx) match {
        case CborItem.Break =>
          Pull.pure((chunk, idx + 1, rest, chunkAcc, CborValue.Map(acc.result(), true)))
        case _ =>
          if (acc.size == Int.MaxValue) {
            raise(new CborParsingException(s"map size is limited to max int (${Int.MaxValue}) elements"), chunkAcc)
          } else {
            parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, key) =>
              parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, value) =>
                acc.update(key, value)
                parseIndefiniteMap(chunk, idx, rest, acc, chunkAcc)
              }
            }
          }
      }
    }

  private def parseByteStrings[F[_]](chunk: Chunk[CborItem],
                                     idx: Int,
                                     rest: Stream[F, CborItem],
                                     acc: ByteVector,
                                     chunkAcc: List[CborValue])(implicit
      F: RaiseThrowable[F]): Pull[F, CborValue, Result[F, CborValue]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) => parseByteStrings(hd, 0, tl, acc, Nil)
        case None           => Pull.raiseError(new CborParsingException("unexpected end of input"))
      }
    } else {
      chunk(idx) match {
        case CborItem.Break =>
          Pull.pure((chunk, idx + 1, rest, chunkAcc, CborValue.ByteString(acc)))
        case CborItem.ByteString(bytes) =>
          if (acc.size + bytes.size < 0L) {
            raise(new CborParsingException(s"byte string size is limited to max long (${Long.MaxValue}) bits"),
                  chunkAcc)
          } else {
            parseByteStrings(chunk, idx + 1, rest, acc ++ bytes, chunkAcc)
          }
        case item =>
          raise(new CborParsingException(s"unexpected item $item"), chunkAcc)
      }
    }

  private def parseTextStrings[F[_]](chunk: Chunk[CborItem],
                                     idx: Int,
                                     rest: Stream[F, CborItem],
                                     acc: StringBuilder,
                                     chunkAcc: List[CborValue])(implicit
      F: RaiseThrowable[F]): Pull[F, CborValue, Result[F, CborValue]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) => parseTextStrings(hd, 0, tl, acc, Nil)
        case None           => Pull.raiseError(new CborParsingException("unexpected end of input"))
      }
    } else {
      chunk(idx) match {
        case CborItem.Break =>
          Pull.pure((chunk, idx + 1, rest, chunkAcc, CborValue.TextString(acc.result())))
        case CborItem.TextString(text) =>
          if (acc.size + text.size < 0) {
            raise(new CborParsingException(s"text string size is limited to max int (${Int.MaxValue}) characters"),
                  chunkAcc)
          } else {
            parseTextStrings(chunk, idx + 1, rest, acc.append(text), chunkAcc)
          }
        case item =>
          raise(new CborParsingException(s"unexpected item $item"), chunkAcc)
      }
    }

  private def parseTags[F[_]](chunk: Chunk[CborItem],
                              idx: Int,
                              rest: Stream[F, CborItem],
                              tags: CborValue => CborValue,
                              chunkAcc: List[CborValue])(implicit
      F: RaiseThrowable[F]): Pull[F, CborValue, Result[F, CborValue => CborValue]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) => parseTags(hd, 0, tl, tags, Nil)
        case None           => Pull.raiseError(new CborParsingException("unexpected end of input"))
      }
    } else {
      chunk(idx) match {
        case CborItem.Tag(tag) => parseTags(chunk, idx + 1, rest, v => tags(CborValue.Tagged(tag, v)), chunkAcc)
        case _                 => Pull.pure((chunk, idx, rest, chunkAcc, tags))
      }
    }

  private val minusOne = BigInt(-1)

  private def parseValue[F[_]](chunk: Chunk[CborItem], idx: Int, rest: Stream[F, CborItem], chunkAcc: List[CborValue])(
      implicit F: RaiseThrowable[F]): Pull[F, CborValue, Result[F, CborValue]] =
    parseTags(chunk, idx, rest, identity, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, tags) =>
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) => parseValue(hd, 0, tl, Nil)
          case None           => Pull.raiseError(new CborParsingException("unexpected end of input"))
        }
      } else {
        chunk(idx) match {
          case CborItem.False     => Pull.pure((chunk, idx + 1, rest, chunkAcc, tags(CborValue.False)))
          case CborItem.True      => Pull.pure((chunk, idx + 1, rest, chunkAcc, tags(CborValue.True)))
          case CborItem.Null      => Pull.pure((chunk, idx + 1, rest, chunkAcc, tags(CborValue.Null)))
          case CborItem.Undefined => Pull.pure((chunk, idx + 1, rest, chunkAcc, tags(CborValue.Undefined)))
          case CborItem.SimpleValue(value) =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc, tags(CborValue.SimpleValue(value))))
          case CborItem.PositiveInt(bytes) =>
            val value = BigInt(bytes.toHex, 16)
            Pull.pure((chunk, idx + 1, rest, chunkAcc, tags(CborValue.Integer(value))))
          case CborItem.NegativeInt(bytes) =>
            val value = minusOne - BigInt(bytes.toHex, 16)
            Pull.pure((chunk, idx + 1, rest, chunkAcc, tags(CborValue.Integer(value))))
          case CborItem.Float16(raw) =>
            Pull.pure(
              (chunk, idx + 1, rest, chunkAcc, tags(CborValue.Float32(HalfFloat.toFloat(raw.toShort(signed = false))))))
          case CborItem.Float32(raw) =>
            Pull.pure(
              (chunk,
               idx + 1,
               rest,
               chunkAcc,
               tags(CborValue.Float32(JFloat.intBitsToFloat(raw.toInt(signed = false))))))
          case CborItem.Float64(raw) =>
            Pull.pure(
              (chunk,
               idx + 1,
               rest,
               chunkAcc,
               tags(CborValue.Float64(JDouble.longBitsToDouble(raw.toLong(signed = false))))))
          case CborItem.ByteString(bytes) =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc, tags(CborValue.ByteString(bytes))))
          case CborItem.StartIndefiniteByteString =>
            parseByteStrings(chunk, idx + 1, rest, ByteVector.empty, chunkAcc).map {
              case (chunk, idx, rest, chunkAcc, bs) =>
                (chunk, idx, rest, chunkAcc, tags(bs))
            }
          case CborItem.TextString(string) =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc, tags(CborValue.TextString(string))))
          case CborItem.StartIndefiniteTextString =>
            parseTextStrings(chunk, idx + 1, rest, new StringBuilder, chunkAcc).map {
              case (chunk, idx, rest, chunkAcc, ts) =>
                (chunk, idx, rest, chunkAcc, tags(ts))
            }
          case CborItem.StartArray(size) =>
            parseArray(chunk, idx + 1, rest, size, new mutable.ListBuffer, chunkAcc).map {
              case (chunk, idx, rest, chunkAcc, array) => (chunk, idx, rest, chunkAcc, tags(array))
            }
          case CborItem.StartIndefiniteArray =>
            parseIndefiniteArray(chunk, idx + 1, rest, new mutable.ListBuffer, chunkAcc).map {
              case (chunk, idx, rest, chunkAcc, array) => (chunk, idx, rest, chunkAcc, tags(array))
            }
          case CborItem.StartMap(size) =>
            parseMap(chunk, idx + 1, rest, size, mutable.Map.empty, chunkAcc).map {
              case (chunk, idx, rest, chunkAcc, array) => (chunk, idx, rest, chunkAcc, tags(array))
            }
          case CborItem.StartIndefiniteMap =>
            parseIndefiniteMap(chunk, idx + 1, rest, mutable.Map.empty, chunkAcc).map {
              case (chunk, idx, rest, chunkAcc, array) => (chunk, idx, rest, chunkAcc, tags(array))
            }
          case item =>
            raise(new CborParsingException(s"unknown item $item"), chunkAcc)
        }
      }
    }

  def pipe[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, CborItem, CborValue] = {
    def go(chunk: Chunk[CborItem],
           idx: Int,
           rest: Stream[F, CborItem],
           chunkAcc: List[CborValue]): Pull[F, CborValue, Unit] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) => go(hd, 0, tl, Nil)
          case None           => Pull.done
        }
      } else {
        parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, value) =>
          go(chunk, idx, rest, value :: chunkAcc)
        }
      }

    s => go(Chunk.empty, 0, s, Nil).stream
  }

}
