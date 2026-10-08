/*
 * Copyright 2024 fs2-data Project
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

import scala.annotation.tailrec

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

  /** Returns the error message if `item` is invalid on its own. */
  private def itemError(item: CborItem): Option[String] =
    item match {
      case CborItem.PositiveInt(bytes) =>
        val size = bytes.size
        if (size <= 8 && validIntSizes(size)) None else Some(s"invalid positive integer size $size")
      case CborItem.NegativeInt(bytes) =>
        val size = bytes.size
        if (size <= 8 && validIntSizes(size)) None else Some(s"invalid negate integer size $size")
      case CborItem.Float16(bytes) =>
        if (bytes.size == 2) None else Some(s"invalid half float size ${bytes.size}")
      case CborItem.Float32(bytes) =>
        if (bytes.size == 4) None else Some(s"invalid float size ${bytes.size}")
      case CborItem.Float64(bytes) =>
        if (bytes.size == 8) None else Some(s"invalid double size ${bytes.size}")
      case CborItem.Break =>
        Some("unexpected break")
      case _ =>
        None
    }

  /** Pushes the context opened by `item`, if any. */
  private def open(item: CborItem, ctx: ValidationContext): ValidationContext =
    item match {
      case CborItem.StartArray(size) if size > 0 => StackElement.Expect(size) :: ctx
      case CborItem.StartMap(size) if size > 0   => StackElement.Expect(size * 2) :: ctx
      case CborItem.StartIndefiniteArray         => StackElement.IndefiniteArray :: ctx
      case CborItem.StartIndefiniteMap           => StackElement.IndefiniteMap :: ctx
      case CborItem.StartIndefiniteTextString    => StackElement.IndefiniteTextString :: ctx
      case CborItem.StartIndefiniteByteString    => StackElement.IndefiniteByteString :: ctx
      case _                                     => ctx
    }

  def pipe[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, CborItem, CborItem] = {

    def raiseAt(chunk: Chunk[CborItem], idx: Int, msg: String): Pull[F, CborItem, Nothing] =
      Pull.output(chunk.take(idx)) >> Pull.raiseError(new CborValidationException(msg))

    // validates `item` and returns the context after it, `ctx` being the enclosing context with `item` already accounted for
    def validate(item: CborItem, ctx: ValidationContext): Either[String, ValidationContext] =
      itemError(item).toLeft(open(item, ctx))

    @tailrec
    def validateChunk(chunk: Chunk[CborItem], idx: Int, ctx: ValidationContext): Pull[F, CborItem, ValidationContext] =
      if (idx >= chunk.size) {
        Pull.output(chunk).as(ctx)
      } else {
        val item = chunk(idx)
        val next: Either[String, ValidationContext] = ctx match {
          case StackElement.IndefiniteTextString :: rest =>
            // only definite text strings are allowed until break
            item match {
              case CborItem.TextString(_) => Right(ctx)
              case CborItem.Break         => Right(rest)
              case _ => Left("only definite size text strings are allowed in indefinite text strings")
            }
          case StackElement.IndefiniteByteString :: rest =>
            // only definite byte strings are allowed until break
            item match {
              case CborItem.ByteString(_) => Right(ctx)
              case CborItem.Break         => Right(rest)
              case _ => Left("only definite size byte strings are allowed in indefinite byte strings")
            }
          case StackElement.IndefiniteArray :: rest =>
            // pop the array if break is encountered
            item match {
              case CborItem.Break => Right(rest)
              case _              => validate(item, ctx)
            }
          case StackElement.IndefiniteMap :: rest =>
            // pop the map if break is encountered or accept key and expect one value at least
            item match {
              case CborItem.Break => Right(rest)
              case _              => validate(item, StackElement.One :: ctx)
            }
          case StackElement.One :: rest =>
            validate(item, rest)
          case StackElement.Expect(n) :: rest =>
            validate(item, StackElement.Expect(n - 1) :: rest)
          case Nil =>
            validate(item, Nil)
        }
        next match {
          case Right(next) => validateChunk(chunk, idx + 1, next)
          case Left(msg)   => raiseAt(chunk, idx, msg)
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
