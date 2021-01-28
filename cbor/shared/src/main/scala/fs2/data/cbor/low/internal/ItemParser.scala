/*
 * Copyright 2020 Lucas Satabin
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
package low
package internal

import cats.syntax.all._

import scala.annotation.switch

import java.lang.{Long => JLong}
import java.nio.charset.StandardCharsets

private[low] object ItemParser {

  private type Context[F[_]] = (Chunk[Byte], Int, Stream[F, Byte], List[CborItem])

  private type Result[F[_], T] = (Chunk[Byte], Int, Stream[F, Byte], List[CborItem], T)

  private def asUnsignedNumber(bytes: Chunk[Byte]) =
    bytes.toBitVector.toLong(signed = false)

  // the resulting chunk contains exactly n bytes
  private def requireBytes[F[_]](chunk: Chunk[Byte],
                                 idx: Int,
                                 rest: Stream[F, Byte],
                                 n: Int,
                                 acc: Chunk.Queue[Byte],
                                 chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Result[F, Chunk[Byte]]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) => requireBytes(hd, 0, tl, n, acc, Nil)
        case None =>
          if (n == 0)
            Pull.pure((Chunk.empty, 0, Stream.empty, Nil, Chunk.empty))
          else
            Pull.raiseError(new CborException("unexpected end of input"))
      }
    } else {
      val seg = chunk.drop(idx).take(n)
      if (seg.size == n) {
        // the chunk was big enough to gather all the bytes, we are done
        Pull.pure((chunk, idx + n, rest, chunkAcc, (acc :+ seg).toChunk))
      } else {
        // accumulate bytes and continue
        requireBytes(chunk, idx + n, rest, n - seg.size, acc :+ seg, chunkAcc)
      }
    }

  private def requireOneByte[F[_]](chunk: Chunk[Byte],
                                   idx: Int,
                                   rest: Stream[F, Byte],
                                   chunkAcc: List[CborItem],
                                   peek: Boolean = false)(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Result[F, Integer]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) => requireOneByte(hd, 0, tl, Nil)
        case None           => Pull.raiseError(new CborException("unexpected end of input"))
      }
    } else {
      Pull.pure((chunk, if (peek) idx else idx + 1, rest, chunkAcc, chunk(idx) & 0xff))
    }

  private def parseInteger[F[_]](chunk: Chunk[Byte],
                                 idx: Int,
                                 rest: Stream[F, Byte],
                                 additional: Byte,
                                 chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Result[F, Chunk[Byte]]] =
    if (additional <= 23) {
      Pull.pure((chunk, idx, rest, chunkAcc, Chunk.singleton(additional)))
    } else if (additional == 24) {
      requireBytes(chunk, idx, rest, 1, Chunk.Queue.empty, chunkAcc)
    } else if (additional == 25) {
      requireBytes(chunk, idx, rest, 2, Chunk.Queue.empty, chunkAcc)
    } else if (additional == 26) {
      requireBytes(chunk, idx, rest, 4, Chunk.Queue.empty, chunkAcc)
    } else if (additional == 27) {
      requireBytes(chunk, idx, rest, 8, Chunk.Queue.empty, chunkAcc)
    } else {
      Pull.raiseError(new CborException(s"unknown additional information $additional"))
    }

  private def parseInnerByteStrings[F[_]](chunk: Chunk[Byte],
                                          idx: Int,
                                          rest: Stream[F, Byte],
                                          chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    requireOneByte(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, byte) =>
      if (byte == 0xff) {
        Pull.pure((chunk, idx, rest, CborItem.Break :: chunkAcc))
      } else {
        val major = (byte & 0xe0) >>> 5
        val additional = (byte & 0x1f).toByte
        if (major == MajorType.ByteString)
          if (additional == 31)
            Pull.raiseError(new CborException("nested indefinite byte strings are not allowed"))
          else
            parseByteString(chunk, idx, rest, additional, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc) =>
              parseInnerByteStrings(chunk, idx, rest, chunkAcc)
            }
        else
          Pull.raiseError(new CborException(s"byte string major type expected but got $major"))
      }
    }

  private def parseByteString[F[_]](chunk: Chunk[Byte],
                                    idx: Int,
                                    rest: Stream[F, Byte],
                                    additional: Byte,
                                    chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    if (additional == 31) {
      parseInnerByteStrings(chunk, idx, rest, CborItem.StartIndefiniteByteString :: chunkAcc)
    } else {
      parseInteger(chunk, idx, rest, additional, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, bytes) =>
        val size = asUnsignedNumber(bytes)
        if (JLong.compareUnsigned(size, Int.MaxValue.toLong) <= 0) {
          requireBytes(chunk, idx, rest, size.toInt, Chunk.Queue.empty, chunkAcc).map {
            case (chunk, idx, rest, chunkAcc, bytes) =>
              (chunk, idx, rest, CborItem.ByteString(bytes.toByteVector) :: chunkAcc)
          }
        } else {
          Pull.raiseError(new CborException(
            s"fixed size strings are limited to max int (${Int.MaxValue}) bytes but got ${JLong.toUnsignedString(size)}"))
        }
      }
    }

  private def parseInnerTextStrings[F[_]](chunk: Chunk[Byte],
                                          idx: Int,
                                          rest: Stream[F, Byte],
                                          chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    requireOneByte(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, byte) =>
      if (byte == 0xff) {
        Pull.pure((chunk, idx, rest, CborItem.Break :: chunkAcc))
      } else {
        val major = (byte & 0xe0) >>> 5
        val additional = (byte & 0x1f).toByte
        if (major == MajorType.TextString)
          if (additional == 31)
            Pull.raiseError(new CborException("nested indefinite byte strings are not allowed"))
          else
            parseTextString(chunk, idx, rest, additional, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc) =>
              parseInnerTextStrings(chunk, idx, rest, chunkAcc)
            }
        else
          Pull.raiseError(new CborException("text string major type expected"))
      }
    }

  private def parseTextString[F[_]](chunk: Chunk[Byte],
                                    idx: Int,
                                    rest: Stream[F, Byte],
                                    additional: Byte,
                                    chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    if (additional == 31) {
      parseInnerTextStrings(chunk, idx, rest, CborItem.StartIndefiniteTextString :: chunkAcc)
    } else {
      parseInteger(chunk, idx, rest, additional, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, bytes) =>
        val size = asUnsignedNumber(bytes)
        if (JLong.compareUnsigned(size, Int.MaxValue.toLong) <= 0) {
          requireBytes(chunk, idx, rest, size.toInt, Chunk.Queue.empty, chunkAcc).flatMap {
            case (chunk, idx, rest, chunkAcc, bytes) =>
              Either.catchNonFatal(new String(bytes.toArray, StandardCharsets.UTF_8)) match {
                case Right(str) => Pull.pure((chunk, idx, rest, CborItem.TextString(str) :: chunkAcc))
                case Left(exn)  => Pull.raiseError(new CborException("malformed text string", exn))
              }
          }
        } else {
          Pull.raiseError(new CborException(
            s"fixed size strings are limited to max int (${Int.MaxValue}) bytes but got ${JLong.toUnsignedString(size)}"))
        }
      }
    }

  private def parseValues[F[_]](chunk: Chunk[Byte],
                                idx: Int,
                                rest: Stream[F, Byte],
                                count: Long,
                                chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    if (JLong.compareUnsigned(count, 0l) == 0) {
      // we are done
      Pull.pure((chunk, idx, rest, chunkAcc))
    } else {
      parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc) =>
        parseValues(chunk, idx, rest, count - 1, chunkAcc)
      }
    }

  private def parseValues[F[_]](chunk: Chunk[Byte], idx: Int, rest: Stream[F, Byte], chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    requireOneByte(chunk, idx, rest, chunkAcc, peek = true).flatMap { case (chunk, idx, rest, chunkAcc, byte) =>
      if (byte == 0xff) {
        Pull.pure((chunk, idx + 1, rest, CborItem.Break :: chunkAcc))
      } else {
        parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc) =>
          parseValues(chunk, idx, rest, chunkAcc)
        }
      }
    }

  private def parseArray[F[_]](chunk: Chunk[Byte],
                               idx: Int,
                               rest: Stream[F, Byte],
                               additional: Byte,
                               chunkAcc: List[CborItem])(implicit F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    if (additional == 31) {
      parseValues(chunk, idx, rest, CborItem.StartIndefiniteArray :: chunkAcc)
    } else {
      parseInteger(chunk, idx, rest, additional, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, bytes) =>
        val size = asUnsignedNumber(bytes)
        parseValues(chunk, idx, rest, size, CborItem.StartArray(size) :: chunkAcc)
      }
    }

  private def parseKeyValues[F[_]](chunk: Chunk[Byte],
                                   idx: Int,
                                   rest: Stream[F, Byte],
                                   count: Long,
                                   chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    if (JLong.compareUnsigned(count, 0l) == 0) {
      // we are done
      Pull.pure((chunk, idx, rest, chunkAcc))
    } else {
      parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc) =>
        parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc) =>
          parseKeyValues(chunk, idx, rest, count - 1, chunkAcc)
        }
      }
    }

  private def parseKeyValues[F[_]](chunk: Chunk[Byte], idx: Int, rest: Stream[F, Byte], chunkAcc: List[CborItem])(
      implicit F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    requireOneByte(chunk, idx, rest, chunkAcc, peek = true).flatMap { case (chunk, idx, rest, chunkAcc, byte) =>
      if (byte == 0xff) {
        Pull.pure((chunk, idx + 1, rest, CborItem.Break :: chunkAcc))
      } else {
        parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc) =>
          parseKeyValues(chunk, idx, rest, chunkAcc)
        }
      }
    }

  private def parseMap[F[_]](chunk: Chunk[Byte],
                             idx: Int,
                             rest: Stream[F, Byte],
                             additional: Byte,
                             chunkAcc: List[CborItem])(implicit F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    if (additional == 31) {
      parseKeyValues(chunk, idx, rest, CborItem.StartIndefiniteMap :: chunkAcc)
    } else {
      parseInteger(chunk, idx, rest, additional, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, bytes) =>
        val size = asUnsignedNumber(bytes)
        parseKeyValues(chunk, idx, rest, size, CborItem.StartMap(size) :: chunkAcc)
      }
    }

  private def parseTaggedValue[F[_]](chunk: Chunk[Byte],
                                     idx: Int,
                                     rest: Stream[F, Byte],
                                     additional: Byte,
                                     chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    parseInteger(chunk, idx, rest, additional, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, bytes) =>
      val tag = asUnsignedNumber(bytes)
      parseValue(chunk, idx, rest, CborItem.Tag(tag) :: chunkAcc)
    }

  private def parseSimple[F[_]](chunk: Chunk[Byte],
                                idx: Int,
                                rest: Stream[F, Byte],
                                additional: Byte,
                                chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    (additional: @switch) match {
      case 20 =>
        Pull.pure((chunk, idx, rest, CborItem.False :: chunkAcc))
      case 21 =>
        Pull.pure((chunk, idx, rest, CborItem.True :: chunkAcc))
      case 22 =>
        Pull.pure((chunk, idx, rest, CborItem.Null :: chunkAcc))
      case 23 =>
        Pull.pure((chunk, idx, rest, CborItem.Undefined :: chunkAcc))
      case 24 =>
        requireOneByte(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, byte) =>
          if (byte >= 0 && byte < 32)
            Pull.raiseError(new CborException(s"invalid simple value additional byte $byte"))
          else
            Pull.pure((chunk, idx, rest, CborItem.SimpleValue(byte.toByte) :: chunkAcc))
        }
      case 25 =>
        requireBytes(chunk, idx, rest, 2, Chunk.Queue.empty[Byte], chunkAcc).map {
          case (chunk, idx, rest, chunkAcc, bytes) =>
            (chunk, idx, rest, CborItem.Float16(bytes.toByteVector) :: chunkAcc)
        }
      case 26 =>
        requireBytes(chunk, idx, rest, 4, Chunk.Queue.empty[Byte], chunkAcc).map {
          case (chunk, idx, rest, chunkAcc, bytes) =>
            (chunk, idx, rest, CborItem.Float32(bytes.toByteVector) :: chunkAcc)
        }
      case 27 =>
        requireBytes(chunk, idx, rest, 8, Chunk.Queue.empty[Byte], chunkAcc).map {
          case (chunk, idx, rest, chunkAcc, bytes) =>
            (chunk, idx, rest, CborItem.Float64(bytes.toByteVector) :: chunkAcc)
        }
      case 31 =>
        Pull.raiseError(new CborException("unexpected break"))
      case _ =>
        if (additional > 27)
          Pull.raiseError(new CborException(s"unassigned 5-bits value $additional"))
        else
          Pull.pure((chunk, idx, rest, CborItem.SimpleValue(additional) :: chunkAcc))
    }

  def parseValue[F[_]](chunk: Chunk[Byte], idx: Int, rest: Stream[F, Byte], chunkAcc: List[CborItem])(implicit
      F: RaiseThrowable[F]): Pull[F, CborItem, Context[F]] =
    requireOneByte(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc, byte) =>
      val major = (byte & 0xe0) >>> 5
      val additional = (byte & 0x1f).toByte
      (major: @switch) match {
        case MajorType.PositiveInteger =>
          parseInteger(chunk, idx, rest, additional, chunkAcc).map { case (chunk, idx, rest, chunkAcc, bytes) =>
            (chunk, idx, rest, CborItem.PositiveInt(bytes.toByteVector) :: chunkAcc)
          }
        case MajorType.NegativeInteger =>
          parseInteger(chunk, idx, rest, additional, chunkAcc).map { case (chunk, idx, rest, chunkAcc, bytes) =>
            (chunk, idx, rest, CborItem.NegativeInt(bytes.toByteVector) :: chunkAcc)
          }
        case MajorType.ByteString  => parseByteString(chunk, idx, rest, additional, chunkAcc)
        case MajorType.TextString  => parseTextString(chunk, idx, rest, additional, chunkAcc)
        case MajorType.Array       => parseArray(chunk, idx, rest, additional, chunkAcc)
        case MajorType.Map         => parseMap(chunk, idx, rest, additional, chunkAcc)
        case MajorType.SemanticTag => parseTaggedValue(chunk, idx, rest, additional, chunkAcc)
        case MajorType.Simple      => parseSimple(chunk, idx, rest, additional, chunkAcc)
        case _                     => Pull.raiseError(new CborException(s"unknown major $major"))
      }
    }

  def pipe[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Byte, CborItem] = {
    def go(chunk: Chunk[Byte], idx: Int, rest: Stream[F, Byte], chunkAcc: List[CborItem]): Pull[F, CborItem, Unit] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) => go(hd, 0, tl, Nil)
          case None           => Pull.done
        }
      } else {
        parseValue(chunk, idx, rest, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc) =>
          go(chunk, idx, rest, chunkAcc)
        }
      }

    go(Chunk.empty, 0, _, Nil).stream
  }

}
