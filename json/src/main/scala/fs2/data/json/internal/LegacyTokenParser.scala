/*
 * Copyright 2023 Lucas Satabin
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
package json
package internals

import fs2.data.text.CharLikeChunks

import scala.annotation.switch

private[json] class LegacyTokenParser[F[_], T, Res](s: Stream[F, T])(implicit
    F: RaiseThrowable[F],
    val T: CharLikeChunks[F, T]) {
  private[this] final def empty = (T.create(Stream.empty), ChunkAccumulator.empty[Res])
  private[this] final val keyAcc = new StringBuilder(TokenParser.keyBufferCapacity)

  private[this] def emitChunk(chunkAcc: ChunkAccumulator[Res]) =
    Pull.output(chunkAcc.chunk())

  // the opening quote has already been read
  private final def string_(context: T.Context,
                            key: Boolean,
                            acc: StringBuilder,
                            chunkAcc: ChunkAccumulator[Res]): Pull[F, Res, (T.Context, ChunkAccumulator[Res])] =
    if (T.needsPull(context)) {
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          chunkAcc.flush()
          string_(context, key, acc, chunkAcc)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      (T.current(context): @switch) match {
        case '"' =>
          val res = acc.result()
          acc.clear()
          if (key) chunkAcc.key(res) else chunkAcc.stringValue(res)
          Pull.pure((T.advance(context), chunkAcc))
        case '\\' =>
          slowString_(T.advance(context), key, StringState.SeenBackslash, 0, acc, chunkAcc)
        case c =>
          if (c >= 0x20 && c <= 0x10ffff)
            string_(T.advance(context), key, acc.append(c), chunkAcc)
          else
            emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"invalid string character '$c'"))
      }
    }

  private final def slowString_(context: T.Context,
                                key: Boolean,
                                state: Int,
                                unicode: Int,
                                acc: StringBuilder,
                                chunkAcc: ChunkAccumulator[Res]): Pull[F, Res, (T.Context, ChunkAccumulator[Res])] = {
    if (T.needsPull(context)) {
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          chunkAcc.flush()
          slowString_(context, key, state, unicode, acc, chunkAcc)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      val c = T.current(context)
      (state: @switch) match {
        case StringState.SeenBackslash =>
          (c: @switch) match {
            case '"'  => slowString_(T.advance(context), key, StringState.Normal, 0, acc.append('"'), chunkAcc)
            case '\\' => slowString_(T.advance(context), key, StringState.Normal, 0, acc.append('\\'), chunkAcc)
            case '/'  => slowString_(T.advance(context), key, StringState.Normal, 0, acc.append('/'), chunkAcc)
            case 'b'  => slowString_(T.advance(context), key, StringState.Normal, 0, acc.append('\b'), chunkAcc)
            case 'f'  => slowString_(T.advance(context), key, StringState.Normal, 0, acc.append('\f'), chunkAcc)
            case 'n'  => slowString_(T.advance(context), key, StringState.Normal, 0, acc.append('\n'), chunkAcc)
            case 'r'  => slowString_(T.advance(context), key, StringState.Normal, 0, acc.append('\r'), chunkAcc)
            case 't'  => slowString_(T.advance(context), key, StringState.Normal, 0, acc.append('\t'), chunkAcc)
            case 'u'  => slowString_(T.advance(context), key, StringState.Expect4Unicode, 0, acc, chunkAcc)
            case _ =>
              emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unknown escaped character '$c'"))
          }
        case StringState.Normal =>
          if (c == '"') {
            val res = acc.result()
            acc.clear()
            if (key) chunkAcc.key(res) else chunkAcc.stringValue(res)
            Pull.pure((T.advance(context), chunkAcc))
          } else if (c == '\\')
            slowString_(T.advance(context), key, StringState.SeenBackslash, 0, acc, chunkAcc)
          else if (c >= 0x20 && c <= 0x10ffff)
            slowString_(T.advance(context), key, StringState.Normal, 0, acc.append(c), chunkAcc)
          else
            emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"invalid string character '$c'"))
        case n /* StringState.ExpectNUnicode */ =>
          val cidx = hexa.indexOf(c.toLower.toInt)
          if (cidx >= 0) {
            val unicode1 = (unicode << 4) | (0x0000000f & cidx)
            if (n == 1) {
              slowString_(T.advance(context),
                          key,
                          StringState.Normal,
                          0,
                          acc.appendAll(Character.toChars(unicode1)),
                          chunkAcc)
            } else {
              slowString_(T.advance(context), key, n - 1, unicode1, acc, chunkAcc)
            }
          } else {
            emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException("malformed escaped unicode sequence"))
          }
      }
    }
  }

  private final def number_(context: T.Context,
                            state: Int,
                            acc: StringBuilder,
                            chunkAcc: ChunkAccumulator[Res]): Pull[F, Res, (T.Context, ChunkAccumulator[Res])] = {
    def step(c: Char, state: Int): Int =
      (c: @switch) match {
        case '-' =>
          (state: @switch) match {
            case NumberState.NumberStart  => NumberState.IntegerStart
            case NumberState.ExponentSign => NumberState.ExponentOne
            case _                        => NumberState.Invalid
          }
        case '0' =>
          (state: @switch) match {
            case NumberState.NumberStart | NumberState.IntegerStart =>
              NumberState.FractionStart
            case NumberState.IntegerBody | NumberState.FractionBody | NumberState.ExponentBody =>
              state
            case NumberState.FractionOne =>
              NumberState.FractionBody
            case NumberState.ExponentSign | NumberState.ExponentOne =>
              NumberState.ExponentBody
            case _ =>
              NumberState.Invalid
          }
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          (state: @switch) match {
            case NumberState.NumberStart | NumberState.IntegerStart =>
              NumberState.IntegerBody
            case NumberState.IntegerBody | NumberState.FractionBody | NumberState.ExponentBody =>
              state
            case NumberState.FractionOne =>
              NumberState.FractionBody
            case NumberState.ExponentSign | NumberState.ExponentOne =>
              NumberState.ExponentBody
            case _ =>
              NumberState.Invalid
          }
        case '.' =>
          (state: @switch) match {
            case NumberState.IntegerBody | NumberState.FractionStart =>
              NumberState.FractionOne
            case _ =>
              NumberState.Invalid
          }
        case 'e' | 'E' =>
          (state: @switch) match {
            case NumberState.IntegerBody | NumberState.FractionStart | NumberState.FractionBody =>
              NumberState.ExponentSign
            case _ =>
              NumberState.Invalid
          }
        case '+' =>
          (state: @switch) match {
            case NumberState.ExponentSign =>
              NumberState.ExponentOne
            case _ =>
              NumberState.Invalid
          }
        case _ =>
          NumberState.Invalid
      }

    if (T.needsPull(context)) {
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          chunkAcc.flush()
          number_(context, state, acc, chunkAcc)
        case None =>
          if (NumberState.isFinal(state))
            Pull.pure(T.create(Stream.empty), chunkAcc.numberValue(acc.result()))
          else
            Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      val c = T.current(context)
      (step(c, state): @switch) match {
        case NumberState.Invalid =>
          if (NumberState.isFinal(state))
            Pull.pure((context, chunkAcc.numberValue(acc.result())))
          else
            emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"invalid number character '$c'"))
        case state =>
          number_(T.advance(context), state, acc.append(c), chunkAcc)
      }
    }
  }

  private final def keyword_(context: T.Context,
                             expected: String,
                             eidx: Int,
                             elen: Int,
                             accumulate: ChunkAccumulator[Res] => ChunkAccumulator[Res],
                             chunkAcc: ChunkAccumulator[Res]): Pull[F, Res, (T.Context, ChunkAccumulator[Res])] = {
    if (T.needsPull(context)) {
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          chunkAcc.flush()
          keyword_(context, expected, eidx, elen, accumulate, chunkAcc)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      val c = T.current(context)
      if (c == expected.charAt(eidx)) {
        if (eidx == elen - 1)
          Pull.pure((T.advance(context), accumulate(chunkAcc)))
        else
          keyword_(T.advance(context), expected, eidx + 1, elen, accumulate, chunkAcc)
      } else {
        emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected character '$c' (expected $expected)"))
      }
    }
  }

  private final def value_(context: T.Context, state: Int, chunkAcc: ChunkAccumulator[Res])(implicit
      F: RaiseThrowable[F]): Pull[F, Res, (T.Context, ChunkAccumulator[Res])] =
    if (T.needsPull(context)) {
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          chunkAcc.flush()
          value_(context, state, chunkAcc)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      val c = T.current(context)
      (c: @switch) match {
        case '{' =>
          Pull.suspend(go_(T.advance(context), State.BeforeObjectKey, chunkAcc.startObject()))
        case '[' =>
          Pull.suspend(go_(T.advance(context), State.BeforeArrayValue, chunkAcc.startArray()))
        case 't' => keyword_(context, "true", 0, 4, _.trueValue(), chunkAcc)
        case 'f' => keyword_(context, "false", 0, 5, _.falseValue(), chunkAcc)
        case 'n' => keyword_(context, "null", 0, 4, _.nullValue(), chunkAcc)
        case '"' => string_(T.advance(context), false, new StringBuilder(TokenParser.stringBufferCapacity), chunkAcc)
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          number_(context, NumberState.NumberStart, new StringBuilder(TokenParser.numberBufferCapacity), chunkAcc)
        case _ => emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c'"))
      }
    }

  private final def go_(context: T.Context,
                        state: Int,
                        chunkAcc: ChunkAccumulator[Res]): Pull[F, Res, (T.Context, ChunkAccumulator[Res])] = {
    if (T.needsPull(context)) {
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          chunkAcc.flush()
          go_(context, state, chunkAcc)
        case None =>
          Pull.pure(empty)
      }
    } else {
      val c = T.current(context)
      (c: @switch) match {
        case ' ' | '\t' | '\r' | '\n' => go_(T.advance(context), state, chunkAcc)
        case _ =>
          (state: @switch) match {
            case State.BeforeValue =>
              value_(context, state, chunkAcc).flatMap { case (context, chunkAcc) =>
                go_(context, State.BeforeValue, chunkAcc)
              }
            case State.BeforeObjectKey =>
              (c: @switch) match {
                case '"' =>
                  string_(T.advance(context), true, keyAcc, chunkAcc)
                    .flatMap { case (context, chunkAcc) => go_(context, State.AfterObjectKey, chunkAcc) }
                case '}' =>
                  Pull.pure((T.advance(context), chunkAcc.endObject()))
                case _ =>
                  emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' before object key"))
              }
            case State.ExpectObjectKey =>
              (c: @switch) match {
                case '"' =>
                  string_(T.advance(context), true, keyAcc, chunkAcc)
                    .flatMap { case (context, chunkAcc) => go_(context, State.AfterObjectKey, chunkAcc) }
                case _ =>
                  emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' before object key"))
              }
            case State.AfterObjectKey =>
              (c: @switch) match {
                case ':' => go_(T.advance(context), State.BeforeObjectValue, chunkAcc)
                case c =>
                  emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' after object key"))
              }
            case State.BeforeObjectValue =>
              value_(context, State.AfterObjectValue, chunkAcc)
                .flatMap { case (context, chunkAcc) => go_(context, State.AfterObjectValue, chunkAcc) }
            case State.AfterObjectValue =>
              (c: @switch) match {
                case ',' =>
                  go_(T.advance(context), State.ExpectObjectKey, chunkAcc)
                case '}' =>
                  Pull.pure((T.advance(context), chunkAcc.endObject()))
                case c =>
                  emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' after object value"))
              }
            case State.ExpectArrayValue =>
              value_(context, State.AfterArrayValue, chunkAcc)
                .flatMap { case (context, chunkAcc) => go_(context, State.AfterArrayValue, chunkAcc) }
            case State.BeforeArrayValue =>
              (c: @switch) match {
                case ']' =>
                  Pull.pure((T.advance(context), chunkAcc.endArray()))
                case _ =>
                  value_(context, State.AfterArrayValue, chunkAcc)
                    .flatMap { case (context, chunkAcc) => go_(context, State.AfterArrayValue, chunkAcc) }
              }
            case State.AfterArrayValue =>
              (c: @switch) match {
                case ']' =>
                  Pull.pure((T.advance(context), chunkAcc.endArray()))
                case ',' =>
                  go_(T.advance(context), State.ExpectArrayValue, chunkAcc)
                case c =>
                  emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' after array value"))
              }
          }
      }
    }
  }

  def parse(chunkAcc: ChunkAccumulator[Res]): Pull[F, Res, Unit] =
    go_(T.create(s), State.BeforeValue, chunkAcc).void

}
