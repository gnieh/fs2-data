package fs2
package data

import cbor.low.CborItem
import json.Token

import scala.collection.mutable.ListBuffer

import scodec.bits.ByteVector

import java.lang.{Float => JFloat, Double => JDouble}

package object cbor {

  private def knownTags: Set[Long] =
    Set(Tags.PositiveBigNum,
        Tags.NegativeBigNum,
        Tags.ExpectedBase64Encoding,
        Tags.ExpectedBase64UrlEncoding,
        Tags.ExpectedBase16Encoding)

  private val minusOne = BigInt(-1)

  /** Transforms the stream of CBOR items into a stream of JSON tokens.
    * The transformation if performed based on rules in [[https://www.rfc-editor.org/rfc/rfc8949.html#name-converting-from-cbor-to-jso section 6.1 of RFC 8949]].
    */
  def jsonTokens[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, CborItem, Token] = {

    def decode(tag: Long, bytes: ByteVector): String =
      tag match {
        case Tags.PositiveBigNum            => bytes.toBase64UrlNoPad
        case Tags.NegativeBigNum            => s"~${bytes.toBase64UrlNoPad}"
        case Tags.ExpectedBase64UrlEncoding => bytes.toBase64UrlNoPad
        case Tags.ExpectedBase64Encoding    => bytes.toBase64
        case Tags.ExpectedBase16Encoding    => bytes.toBase16
        case _                              => bytes.toBase64UrlNoPad
      }

    def tokenizeTextStrings(chunk: Chunk[CborItem],
                            idx: Int,
                            rest: Stream[F, CborItem],
                            acc: StringBuilder,
                            chunkAcc: ListBuffer[Token])
        : Pull[F, Token, (Chunk[CborItem], Int, Stream[F, CborItem], ListBuffer[Token], String)] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.result())) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.clear()
            tokenizeTextStrings(hd, 0, tl, acc, chunkAcc)
          case None => Pull.raiseError(new CborParsingException("unexpected end of input"))
        }
      } else {
        chunk(idx) match {
          case CborItem.TextString(s) => tokenizeTextStrings(chunk, idx + 1, rest, acc.append(s), chunkAcc)
          case CborItem.Break         => Pull.pure((chunk, idx + 1, rest, chunkAcc, acc.result()))
          case t                      => Pull.raiseError(new CborParsingException(s"unexpected CBOR item $t"))
        }
      }

    def tokenizeByteStrings(chunk: Chunk[CborItem],
                            idx: Int,
                            rest: Stream[F, CborItem],
                            tag: Long,
                            acc: StringBuilder,
                            chunkAcc: ListBuffer[Token])
        : Pull[F, Token, (Chunk[CborItem], Int, Stream[F, CborItem], ListBuffer[Token], String)] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.result())) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.clear()
            tokenizeByteStrings(hd, 0, tl, tag, acc, chunkAcc)
          case None => Pull.raiseError(new CborParsingException("unexpected end of input"))
        }
      } else {
        chunk(idx) match {
          case CborItem.ByteString(s) =>
            tokenizeByteStrings(chunk, idx + 1, rest, tag, acc.append(decode(tag, s)), chunkAcc)
          case CborItem.Break => Pull.pure((chunk, idx + 1, rest, chunkAcc, acc.result()))
          case t              => Pull.raiseError(new CborParsingException(s"unexpected CBOR item $t"))
        }
      }

    def tokenizeMap(
        chunk: Chunk[CborItem],
        idx: Int,
        rest: Stream[F, CborItem],
        tag: Option[Long],
        count: Long,
        chunkAcc: ListBuffer[Token]): Pull[F, Token, (Chunk[CborItem], Int, Stream[F, CborItem], ListBuffer[Token])] =
      if (count == 0L) {
        Pull.pure((chunk, idx, rest, chunkAcc += Token.EndObject))
      } else if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.result())) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.clear()
            tokenizeMap(hd, 0, tl, tag, count, chunkAcc)
          case None => Pull.raiseError(new CborParsingException("unexpected end of input"))
        }
      } else {
        chunk(idx) match {
          case CborItem.Break if count == -1L =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += Token.EndObject))
          case CborItem.TextString(k) =>
            tokenizeValue(chunk, idx + 1, rest, tag, chunkAcc += Token.Key(k)).flatMap {
              case (chunk, idx, rest, chunkAcc) =>
                tokenizeMap(chunk, idx, rest, tag, math.max(-1L, count - 1L), chunkAcc)
            }
          case CborItem.StartIndefiniteTextString =>
            tokenizeTextStrings(chunk, idx + 1, rest, new StringBuilder, chunkAcc).flatMap {
              case (chunk, idx, rest, chunkAcc, s) =>
                tokenizeValue(chunk, idx, rest, None, chunkAcc += Token.Key(s)).flatMap {
                  case (chunk, idx, rest, chunkAcc) =>
                    tokenizeMap(chunk, idx, rest, tag, math.max(-1L, count - 1L), chunkAcc)
                }
            }
          case CborItem.ByteString(bytes) =>
            tokenizeValue(chunk, idx + 1, rest, None, chunkAcc += Token.Key(decode(tag.getOrElse(-1), bytes)))
              .flatMap { case (chunk, idx, rest, chunkAcc) =>
                tokenizeMap(chunk, idx, rest, tag, math.max(-1L, count - 1L), chunkAcc)
              }
          case CborItem.StartIndefiniteByteString =>
            tokenizeByteStrings(chunk, idx + 1, rest, tag.getOrElse(-1), new StringBuilder, chunkAcc).flatMap {
              case (chunk, idx, rest, chunkAcc, s) =>
                tokenizeValue(chunk, idx, rest, None, chunkAcc += Token.Key(s)).flatMap {
                  case (chunk, idx, rest, chunkAcc) =>
                    tokenizeMap(chunk, idx, rest, tag, math.max(-1L, count - 1L), chunkAcc)
                }
            }
          case t =>
            Pull.raiseError(new CborParsingException(s"unepexted CBOR item $t for key"))
        }
      }

    def tokenizeArray(
        chunk: Chunk[CborItem],
        idx: Int,
        rest: Stream[F, CborItem],
        tag: Option[Long],
        count: Long,
        chunkAcc: ListBuffer[Token]): Pull[F, Token, (Chunk[CborItem], Int, Stream[F, CborItem], ListBuffer[Token])] =
      if (count == 0L) {
        Pull.pure((chunk, idx, rest, chunkAcc += Token.EndArray))
      } else if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.result())) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.clear()
            tokenizeArray(hd, 0, tl, tag, count, chunkAcc)
          case None => Pull.raiseError(new CborParsingException("unexpected end of input"))
        }
      } else {
        chunk(idx) match {
          case CborItem.Break if count == -1L =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += Token.EndArray))
          case _ =>
            tokenizeValue(chunk, idx, rest, tag, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc) =>
              tokenizeArray(chunk, idx, rest, tag, math.max(-1L, count - 1L), chunkAcc)
            }
        }
      }

    def tokenizeValue(
        chunk: Chunk[CborItem],
        idx: Int,
        rest: Stream[F, CborItem],
        tag: Option[Long],
        chunkAcc: ListBuffer[Token]): Pull[F, Token, (Chunk[CborItem], Int, Stream[F, CborItem], ListBuffer[Token])] = {
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.result())) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.clear()
            tokenizeValue(hd, 0, tl, tag, chunkAcc)
          case None =>
            Pull.raiseError(new CborParsingException("unexpected end of input"))
        }
      } else {
        val item = chunk(idx)
        item match {
          case CborItem.Tag(t) =>
            if (knownTags.contains(t))
              tokenizeValue(chunk, idx + 1, rest, Some(t), chunkAcc)
            else
              tokenizeValue(chunk, idx + 1, rest, tag, chunkAcc)
          case CborItem.TextString(s) =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += Token.StringValue(s)))
          case CborItem.StartIndefiniteTextString =>
            tokenizeTextStrings(chunk, idx + 1, rest, new StringBuilder, chunkAcc).map {
              case (chunk, idx, rest, chunkAcc, s) =>
                (chunk, idx, rest, chunkAcc += Token.StringValue(s))
            }
          case CborItem.ByteString(bytes) =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += Token.StringValue(decode(tag.getOrElse(-1), bytes))))
          case CborItem.StartIndefiniteByteString =>
            tokenizeByteStrings(chunk, idx + 1, rest, tag.getOrElse(-1), new StringBuilder, chunkAcc).map {
              case (chunk, idx, rest, chunkAcc, s) =>
                (chunk, idx, rest, chunkAcc += Token.StringValue(s))
            }
          case CborItem.False =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += Token.FalseValue))
          case CborItem.True =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += Token.TrueValue))
          case CborItem.Null =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += Token.NullValue))
          case CborItem.Undefined =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += Token.StringValue("undefined")))
          case CborItem.SimpleValue(value) =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += Token.StringValue(s"simple(${value & 0xff})")))
          case CborItem.PositiveInt(bytes) =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += Token.NumberValue(BigInt(bytes.toHex, 16).toString(10))))
          case CborItem.NegativeInt(bytes) =>
            Pull.pure(
              (chunk, idx + 1, rest, chunkAcc += Token.NumberValue((minusOne - BigInt(bytes.toHex, 16)).toString(10))))
          case CborItem.Float16(raw) =>
            val hf = HalfFloat.toFloat(raw.toShort(signed = false))
            Pull.pure(
              (chunk,
               idx + 1,
               rest,
               chunkAcc += (if (hf.isFinite) Token.NumberValue(hf.toString()) else Token.StringValue(hf.toString()))))
          case CborItem.Float32(raw) =>
            val f = JFloat.intBitsToFloat(raw.toInt(signed = false))
            Pull.pure(
              (chunk,
               idx + 1,
               rest,
               chunkAcc += (if (f.isFinite) Token.NumberValue(f.toString) else Token.StringValue(f.toString))))
          case CborItem.Float64(raw) =>
            val d = JDouble.longBitsToDouble(raw.toLong(signed = false))
            Pull.pure(
              (chunk,
               idx + 1,
               rest,
               chunkAcc += (if (d.isFinite) Token.NumberValue(d.toString()) else Token.StringValue(d.toString()))))
          case CborItem.StartArray(size) =>
            tokenizeArray(chunk, idx + 1, rest, tag, size, chunkAcc += Token.StartArray)
          case CborItem.StartIndefiniteArray =>
            tokenizeArray(chunk, idx + 1, rest, tag, -1, chunkAcc += Token.StartArray)
          case CborItem.StartMap(size) =>
            tokenizeMap(chunk, idx + 1, rest, tag, size, chunkAcc += Token.StartObject)
          case CborItem.StartIndefiniteMap =>
            tokenizeMap(chunk, idx + 1, rest, tag, -1, chunkAcc += Token.StartObject)
          case CborItem.Break =>
            Pull.raiseError(new CborParsingException("unexpected break"))
        }
      }
    }

    def go(chunk: Chunk[CborItem],
           idx: Int,
           rest: Stream[F, CborItem],
           chunkAcc: ListBuffer[Token]): Pull[F, Token, Unit] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.seq(chunkAcc.result())) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.clear()
            go(hd, 0, tl, chunkAcc)
          case None => Pull.done
        }
      } else {
        tokenizeValue(chunk, idx, rest, None, chunkAcc).flatMap { case (chunk, idx, rest, chunkAcc) =>
          go(chunk, idx, rest, chunkAcc)
        }
      }

    s => go(Chunk.empty, 0, s, new ListBuffer).stream
  }

}
