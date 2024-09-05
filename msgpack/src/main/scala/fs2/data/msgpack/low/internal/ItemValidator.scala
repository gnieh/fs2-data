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
package msgpack
package low
package internal

case class ValidationErrorAt(at: Long, msg: String) extends Error(s"at position ${at}: ${msg}")
case class ValidationError(msg: String) extends Exception(msg)

private[low] object ItemValidator {

  case class Expect(n: Int, from: Long) {
    def dec = Expect(n - 1, from)
  }

  type ValidationContext = (Chunk[MsgpackItem], Int, Long, List[Expect])

  def none[F[_]]: Pipe[F, MsgpackItem, MsgpackItem] = in => in

  def simple[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, MsgpackItem, MsgpackItem] = { in =>
    def step1(chunk: Chunk[MsgpackItem], idx: Int, position: Long): Pull[F, MsgpackItem, Option[Expect]] =
      chunk(idx) match {
        case MsgpackItem.UnsignedInt(bytes) =>
          if (bytes.size > 8) Pull.raiseError(new ValidationErrorAt(position, "Unsigned int exceeds 64 bits"))
          else Pull.pure(None)

        case MsgpackItem.SignedInt(bytes) =>
          if (bytes.size > 8) Pull.raiseError(new ValidationErrorAt(position, "Signed int exceeds 64 bits"))
          else Pull.pure(None)

        case MsgpackItem.Float32(_) =>
          Pull.pure(None)

        case MsgpackItem.Float64(_) =>
          Pull.pure(None)

        case MsgpackItem.Str(bytes) =>
          if (bytes.size > Math.pow(2, 32) - 1)
            Pull.raiseError(new ValidationErrorAt(position, "String exceeds (2^32)-1 bytes"))
          else
            Pull.pure(None)

        case MsgpackItem.Bin(bytes) =>
          if (bytes.size > Math.pow(2, 32) - 1)
            Pull.raiseError(new ValidationErrorAt(position, "Bin exceeds (2^32)-1 bytes"))
          else
            Pull.pure(None)

        case MsgpackItem.Array(size) =>
          if (size == 0)
            Pull.pure(None)
          else
            Pull.pure(Some(Expect(size, position)))

        case MsgpackItem.Map(size) =>
          if (size < 0)
            Pull.raiseError(new ValidationErrorAt(position, s"Map has a negative size ${size}"))
          else if (size == 0)
            Pull.pure(None)
          else
            Pull.pure(Some(Expect(size * 2, position)))

        case MsgpackItem.Extension(_, bytes) =>
          if (bytes.size > Math.pow(2, 32) - 1)
            Pull.raiseError(new ValidationErrorAt(position, "Extension data exceeds (2^32)-1 bytes"))
          else
            Pull.pure(None)

        case _: MsgpackItem.Timestamp32 =>
          Pull.pure(None)

        case item: MsgpackItem.Timestamp64 =>
          if (item.nanoseconds > 999999999)
            Pull.raiseError(
              new ValidationErrorAt(position, "Timestamp64 nanoseconds cannot be larger than '999999999'"))
          else
            Pull.pure(None)

        case MsgpackItem.Timestamp96(nanoseconds, _) =>
          if (nanoseconds > 999999999)
            Pull.raiseError(
              new ValidationErrorAt(position, "Timestamp96 nanoseconds cannot be larger than '999999999'"))
          else
            Pull.pure(None)

        case MsgpackItem.Nil =>
          Pull.pure(None)

        case MsgpackItem.True =>
          Pull.pure(None)

        case MsgpackItem.False =>
          Pull.pure(None)
      }

    def stepChunk(chunk: Chunk[MsgpackItem],
                  idx: Int,
                  stream: Stream[F, MsgpackItem],
                  position: Long,
                  state: List[Expect]): Pull[F, MsgpackItem, ValidationContext] = {
      if (idx >= chunk.size)
        Pull.output(chunk).as((Chunk.empty, 0, position, state))
      else
        step1(chunk, idx, position).flatMap { el =>
          val stateNew: List[Expect] =
            if (state.isEmpty)
              state
            else if (state.head.n == 1)
              state.tail
            else
              state.head.dec :: state.tail

          val prepended = el match {
            case Some(x) => x :: stateNew
            case None    => stateNew
          }

          stepChunk(chunk, idx + 1, stream, position + 1, prepended)
        }
    }

    def go(stream: Stream[F, MsgpackItem], idx: Int, position: Long, state: List[Expect]): Pull[F, MsgpackItem, Unit] =
      stream.pull.uncons.flatMap {
        case Some((chunk, stream)) =>
          stepChunk(chunk, idx, stream, position, state).flatMap { case (_, idx, position, state) =>
            go(stream, idx, position, state)
          }
        case None =>
          if (state.isEmpty)
            Pull.done
          else
            Pull.raiseError(new ValidationError(s"Unexpected end of input (starting at ${state.head.from})"))
      }

    go(in, 0, 0, List.empty).stream
  }

}
