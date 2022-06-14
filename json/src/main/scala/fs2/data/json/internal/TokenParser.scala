/*
 * Copyright 2019-2022 Lucas Satabin
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

import text._

import scala.collection.immutable.VectorBuilder
import scala.annotation.switch

private[json] object TokenParser {

  def pipe[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): Pipe[F, T, Token] = { s =>
    // the opening quote has already been read
    def string_(context: T.Context,
                key: Boolean,
                state: Int,
                unicode: Int,
                acc: StringBuilder,
                chunkAcc: VectorBuilder[Token]): Pull[F, Token, Option[(T.Context, VectorBuilder[Token])]] = {
      if (T.needsPull(context)) {
        emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
          case Some(context) =>
            chunkAcc.clear()
            string_(context, key, state, unicode, acc, chunkAcc)
          case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
      } else {
        val c = T.current(context)
        (state: @switch) match {
          case StringState.SeenBackslash =>
            (c: @switch) match {
              case '"'  => string_(T.advance(context), key, StringState.Normal, 0, acc.append('"'), chunkAcc)
              case '\\' => string_(T.advance(context), key, StringState.Normal, 0, acc.append('\\'), chunkAcc)
              case '/'  => string_(T.advance(context), key, StringState.Normal, 0, acc.append('/'), chunkAcc)
              case 'b'  => string_(T.advance(context), key, StringState.Normal, 0, acc.append('\b'), chunkAcc)
              case 'f'  => string_(T.advance(context), key, StringState.Normal, 0, acc.append('\f'), chunkAcc)
              case 'n'  => string_(T.advance(context), key, StringState.Normal, 0, acc.append('\n'), chunkAcc)
              case 'r'  => string_(T.advance(context), key, StringState.Normal, 0, acc.append('\r'), chunkAcc)
              case 't'  => string_(T.advance(context), key, StringState.Normal, 0, acc.append('\t'), chunkAcc)
              case 'u'  => string_(T.advance(context), key, StringState.Expect4Unicode, 0, acc, chunkAcc)
              case _ => emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unknown escaped character '$c'"))
            }
          case StringState.Normal =>
            if (c == '"')
              Pull.pure(
                Some(
                  (T.advance(context),
                   chunkAcc += (if (key) Token.Key(acc.result()) else Token.StringValue(acc.result())))))
            else if (c == '\\')
              string_(T.advance(context), key, StringState.SeenBackslash, 0, acc, chunkAcc)
            else if (c >= 0x20 && c <= 0x10ffff)
              string_(T.advance(context), key, StringState.Normal, 0, acc.append(c), chunkAcc)
            else
              emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"invalid string character '$c'"))
          case n /* StringState.ExpectNUnicode */ =>
            val cidx = hexa.indexOf(c.toLower)
            if (cidx >= 0) {
              val unicode1 = (unicode << 4) | (0x0000000f & cidx)
              if (n == 1) {
                string_(T.advance(context),
                        key,
                        StringState.Normal,
                        0,
                        acc.appendAll(Character.toChars(unicode1)),
                        chunkAcc)
              } else {
                string_(T.advance(context), key, n - 1, unicode1, acc, chunkAcc)
              }
            } else {
              emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException("malformed escaped unicode sequence"))
            }
        }
      }
    }

    def number_(context: T.Context,
                state: Int,
                acc: StringBuilder,
                chunkAcc: VectorBuilder[Token]): Pull[F, Token, Option[(T.Context, VectorBuilder[Token])]] = {
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
            chunkAcc.clear()
            number_(context, state, acc, chunkAcc)
          case None =>
            if (NumberState.isFinal(state))
              Pull.output1(Token.NumberValue(acc.result())) >> Pull.pure(None)
            else
              Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
      } else {
        val c = T.current(context)
        (step(c, state): @switch) match {
          case NumberState.Invalid =>
            if (NumberState.isFinal(state))
              Pull.pure(Some((context, chunkAcc += Token.NumberValue(acc.result()))))
            else
              emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"invalid number character '$c'"))
          case state =>
            number_(T.advance(context), state, acc.append(c), chunkAcc)
        }
      }
    }

    def keyword_(context: T.Context,
                 expected: String,
                 eidx: Int,
                 token: Token,
                 chunkAcc: VectorBuilder[Token]): Pull[F, Token, Option[(T.Context, VectorBuilder[Token])]] = {
      if (T.needsPull(context)) {
        emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
          case Some(context) =>
            chunkAcc.clear()
            keyword_(context, expected, eidx, token, chunkAcc)
          case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
      } else {
        val c = T.current(context)
        if (c == expected.charAt(eidx)) {
          if (eidx == expected.length - 1)
            Pull.pure(Some((T.advance(context), chunkAcc += token)))
          else
            keyword_(T.advance(context), expected, eidx + 1, token, chunkAcc)
        } else {
          emitChunk(chunkAcc) >> Pull.raiseError[F](
            new JsonException(s"unexpected character '$c' (expected $expected)"))
        }
      }
    }

    def value_(context: T.Context, state: Int, chunkAcc: VectorBuilder[Token])(implicit
        F: RaiseThrowable[F]): Pull[F, Token, Option[(T.Context, VectorBuilder[Token])]] =
      if (T.needsPull(context)) {
        emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
          case Some(context) =>
            chunkAcc.clear()
            value_(context, state, chunkAcc)
          case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
      } else {
        val c = T.current(context)
        (c: @switch) match {
          case '{' =>
            Pull.suspend(go_(T.advance(context), State.BeforeObjectKey, chunkAcc += Token.StartObject))
          case '[' =>
            Pull.suspend(go_(T.advance(context), State.BeforeArrayValue, chunkAcc += Token.StartArray))
          case 't' => keyword_(context, "true", 0, Token.TrueValue, chunkAcc)
          case 'f' => keyword_(context, "false", 0, Token.FalseValue, chunkAcc)
          case 'n' => keyword_(context, "null", 0, Token.NullValue, chunkAcc)
          case '"' => string_(T.advance(context), false, StringState.Normal, 0, new StringBuilder, chunkAcc)
          case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            number_(context, NumberState.NumberStart, new StringBuilder, chunkAcc)
          case c => emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c'"))
        }
      }

    def continue(state: Int)(result: Option[(T.Context, VectorBuilder[Token])])(implicit F: RaiseThrowable[F]) =
      result match {
        case Some((context, chunkAcc)) =>
          go_(context, state, chunkAcc)
        case None =>
          Pull.pure(None)
      }

    def go_(context: T.Context,
            state: Int,
            chunkAcc: VectorBuilder[Token]): Pull[F, Token, Option[(T.Context, VectorBuilder[Token])]] = {
      if (T.needsPull(context)) {
        emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
          case Some(context) =>
            chunkAcc.clear()
            go_(context, state, chunkAcc)
          case None =>
            Pull.pure(None)
        }
      } else {
        val c = T.current(context)
        (c: @switch) match {
          case ' ' | '\t' | '\r' | '\n' => go_(T.advance(context), state, chunkAcc)
          case _ =>
            (state: @switch) match {
              case State.BeforeValue =>
                value_(context, state, chunkAcc).flatMap(res => continue(State.BeforeValue)(res))
              case State.BeforeObjectKey =>
                (c: @switch) match {
                  case '"' =>
                    string_(T.advance(context), true, StringState.Normal, 0, new StringBuilder, chunkAcc)
                      .flatMap(res => continue(State.AfterObjectKey)(res))
                  case '}' =>
                    Pull.pure(Some((T.advance(context), chunkAcc += Token.EndObject)))
                  case _ =>
                    emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' before object key"))
                }
              case State.ExpectObjectKey =>
                (c: @switch) match {
                  case '"' =>
                    string_(T.advance(context), true, StringState.Normal, 0, new StringBuilder, chunkAcc)
                      .flatMap(res => continue(State.AfterObjectKey)(res))
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
                  .flatMap(res => continue(State.AfterObjectValue)(res))
              case State.AfterObjectValue =>
                (c: @switch) match {
                  case ',' =>
                    go_(T.advance(context), State.ExpectObjectKey, chunkAcc)
                  case '}' =>
                    Pull.pure(Some((T.advance(context), chunkAcc += Token.EndObject)))
                  case c =>
                    emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' after object value"))
                }
              case State.ExpectArrayValue =>
                value_(context, State.AfterArrayValue, chunkAcc)
                  .flatMap(res => continue(State.AfterArrayValue)(res))
              case State.BeforeArrayValue =>
                (c: @switch) match {
                  case ']' =>
                    Pull.pure(Some((T.advance(context), chunkAcc += Token.EndArray)))
                  case c =>
                    value_(context, State.AfterArrayValue, chunkAcc)
                      .flatMap(res => continue(State.AfterArrayValue)(res))
                }
              case State.AfterArrayValue =>
                (c: @switch) match {
                  case ']' =>
                    Pull.pure(Some((T.advance(context), chunkAcc += Token.EndArray)))
                  case ',' =>
                    go_(T.advance(context), State.ExpectArrayValue, chunkAcc)
                  case c =>
                    emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' after array value"))
                }
            }
        }
      }
    }

    Stream.suspend(Stream.emit(T.create(s))).flatMap(go_(_, State.BeforeValue, new VectorBuilder).void.stream)
  }
}
