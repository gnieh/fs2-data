/*
 * Copyright 2021 Lucas Satabin
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

import scodec.bits._

private[cbor] object ItemValidator {

  type ValidationContext = List[StackElement]

  sealed trait StackElement
  object StackElement {
    case class Expect(n: Long) extends StackElement
    case object IndefiniteArray extends StackElement
    case object IndefiniteMap extends StackElement
    case object IndefiniteTextString extends StackElement
    case object IndefiniteByteString extends StackElement

    val One = Expect(1)
  }

  val validIntSizes = bin"111010001"

  def pipe[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, CborItem, CborItem] = {

    def raiseAt(chunk: Chunk[CborItem], idx: Int, msg: String): Pull[F, CborItem, Nothing] =
      Pull.output(chunk.take(idx)) >> Pull.raiseError(new CborValidationException(msg))

    def validate(item: CborItem,
                 chunk: Chunk[CborItem],
                 idx: Int,
                 ctx: ValidationContext): Pull[F, CborItem, ValidationContext] =
      item match {
        case CborItem.PositiveInt(bytes) =>
          val size = bytes.size
          if (size <= 8 && validIntSizes(size))
            validateChunk(chunk, idx + 1, ctx)
          else
            raiseAt(chunk, idx, s"invalid positive integer size $size")
        case CborItem.NegativeInt(bytes) =>
          val size = bytes.size
          if (size <= 8 && validIntSizes(size))
            validateChunk(chunk, idx + 1, ctx)
          else
            raiseAt(chunk, idx, s"invalid negate integer size $size")
        case CborItem.Float16(bytes) =>
          if (bytes.size == 2)
            validateChunk(chunk, idx + 1, ctx)
          else
            raiseAt(chunk, idx, s"invalid half float size ${bytes.size}")
        case CborItem.Float32(bytes) =>
          if (bytes.size == 4)
            validateChunk(chunk, idx + 1, ctx)
          else
            raiseAt(chunk, idx, s"invalid float size ${bytes.size}")
        case CborItem.Float64(bytes) =>
          if (bytes.size == 8)
            validateChunk(chunk, idx + 1, ctx)
          else
            raiseAt(chunk, idx, s"invalid double size ${bytes.size}")
        case CborItem.StartArray(size) =>
          if (size > 0)
            validateChunk(chunk, idx + 1, StackElement.Expect(size) :: ctx)
          else
            validateChunk(chunk, idx + 1, ctx)
        case CborItem.StartMap(size) =>
          if (size > 0)
            validateChunk(chunk, idx + 1, StackElement.Expect(size * 2) :: ctx)
          else
            validateChunk(chunk, idx + 1, ctx)
        case CborItem.StartIndefiniteArray =>
          validateChunk(chunk, idx + 1, StackElement.IndefiniteArray :: ctx)
        case CborItem.StartIndefiniteMap =>
          validateChunk(chunk, idx + 1, StackElement.IndefiniteMap :: ctx)
        case CborItem.StartIndefiniteTextString =>
          validateChunk(chunk, idx + 1, StackElement.IndefiniteTextString :: ctx)
        case CborItem.StartIndefiniteByteString =>
          validateChunk(chunk, idx + 1, StackElement.IndefiniteByteString :: ctx)
        case CborItem.Break =>
          raiseAt(chunk, idx, "unexpected break")
        case _ =>
          validateChunk(chunk, idx + 1, ctx)
      }

    def validateChunk(chunk: Chunk[CborItem], idx: Int, ctx: ValidationContext): Pull[F, CborItem, ValidationContext] =
      if (idx >= chunk.size) {
        Pull.output(chunk).as(ctx)
      } else {
        val item = chunk(idx)
        ctx match {
          case StackElement.IndefiniteTextString :: rest =>
            // only definite text strings are allowed until break
            item match {
              case CborItem.TextString(_) => validateChunk(chunk, idx + 1, ctx)
              case CborItem.Break         => validateChunk(chunk, idx + 1, rest)
              case _                      => raiseAt(chunk, idx, "only definite size text strings are allowed in indefinite text strings")
            }
          case StackElement.IndefiniteByteString :: rest =>
            // only definite text strings are allowed until break
            item match {
              case CborItem.ByteString(_) => validateChunk(chunk, idx + 1, ctx)
              case CborItem.Break         => validateChunk(chunk, idx + 1, rest)
              case _                      => raiseAt(chunk, idx, "only definite size byte strings are allowed in indefinite byte strings")
            }
          case StackElement.IndefiniteArray :: rest =>
            // pop the array if break is encountered
            item match {
              case CborItem.Break => validateChunk(chunk, idx + 1, rest)
              case _              => validate(item, chunk, idx, ctx)
            }
          case StackElement.IndefiniteMap :: rest =>
            // pop the map if break is encountered or accept key and expect one value at least
            item match {
              case CborItem.Break => validateChunk(chunk, idx + 1, rest)
              case _              => validate(item, chunk, idx, StackElement.One :: ctx)
            }
          case StackElement.One :: rest =>
            validate(item, chunk, idx, rest)
          case StackElement.Expect(n) :: rest =>
            validate(item, chunk, idx, StackElement.Expect(n - 1) :: rest)
          case Nil =>
            validate(item, chunk, idx, Nil)
        }
      }

    def go(s: Stream[F, CborItem], ctx: ValidationContext): Pull[F, CborItem, Unit] =
      s.pull.uncons.flatMap {
        case Some((hd, tl)) =>
          validateChunk(hd, 0, ctx).flatMap(go(tl, _))
        case None =>
          if (ctx.isEmpty)
            Pull.done
          else
            Pull.raiseError(new CborValidationException("unexpected end of CBOR item stream"))
      }

    go(_, Nil).stream
  }

}
