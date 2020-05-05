/*
 * Copyright 2019 Lucas Satabin
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

import scala.annotation.switch

private[json] object TokenParser {
  // the opening quote has already been read
  def string_[F[_]](
      chunk: Chunk[Char],
      idx: Int,
      rest: Stream[F, Char],
      key: Boolean,
      state: Int,
      unicode: Int,
      acc: StringBuilder,
      chunkAcc: List[Token])(implicit F: RaiseThrowable[F]): Pull[F, Token, Result[F, Char, List[Token]]] = {
    if (idx >= chunk.size)
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((chunk, rest)) => string_(chunk, 0, rest, key, state, unicode, acc, Nil)
        case None                => Pull.raiseError[F](new JsonException("unexpected end of input"))
      } else {
      val c = chunk(idx)
      (state: @switch) match {
        case StringState.SeenBackslash =>
          (c: @switch) match {
            case '"'  => string_(chunk, idx + 1, rest, key, StringState.Normal, 0, acc.append('"'), chunkAcc)
            case '\\' => string_(chunk, idx + 1, rest, key, StringState.Normal, 0, acc.append('\\'), chunkAcc)
            case '/'  => string_(chunk, idx + 1, rest, key, StringState.Normal, 0, acc.append('/'), chunkAcc)
            case 'b'  => string_(chunk, idx + 1, rest, key, StringState.Normal, 0, acc.append('\b'), chunkAcc)
            case 'f'  => string_(chunk, idx + 1, rest, key, StringState.Normal, 0, acc.append('\f'), chunkAcc)
            case 'n'  => string_(chunk, idx + 1, rest, key, StringState.Normal, 0, acc.append('\n'), chunkAcc)
            case 'r'  => string_(chunk, idx + 1, rest, key, StringState.Normal, 0, acc.append('\r'), chunkAcc)
            case 't'  => string_(chunk, idx + 1, rest, key, StringState.Normal, 0, acc.append('\t'), chunkAcc)
            case 'u'  => string_(chunk, idx + 1, rest, key, StringState.Expect4Unicode, 0, acc, chunkAcc)
            case _    => Pull.raiseError[F](new JsonException(s"unknown escaped character '$c'"))
          }
        case StringState.Normal =>
          if (c == '"')
            Pull.pure(Some(
              (chunk, idx + 1, rest, (if (key) Token.Key(acc.result) else Token.StringValue(acc.result)) :: chunkAcc)))
          else if (c == '\\')
            string_(chunk, idx + 1, rest, key, StringState.SeenBackslash, 0, acc, chunkAcc)
          else if (c >= 0x20 && c <= 0x10ffff)
            string_(chunk, idx + 1, rest, key, StringState.Normal, 0, acc.append(c), chunkAcc)
          else
            Pull.raiseError[F](new JsonException(s"invalid string character '$c'"))
        case n /* StringState.ExpectNUnicode */ =>
          val cidx = hexa.indexOf(c.toLower)
          if (cidx >= 0) {
            val unicode1 = (unicode << 4) | (0x0000000f & cidx)
            if (n == 1) {
              string_(chunk,
                      idx + 1,
                      rest,
                      key,
                      StringState.Normal,
                      0,
                      acc.appendAll(Character.toChars(unicode1)),
                      chunkAcc)
            } else {
              string_(chunk, idx + 1, rest, key, n - 1, unicode1, acc, chunkAcc)
            }
          } else {
            Pull.raiseError[F](new JsonException("malformed escaped unicode sequence"))
          }
      }
    }
  }

  def number_[F[_]](
      chunk: Chunk[Char],
      idx: Int,
      rest: Stream[F, Char],
      state: Int,
      acc: StringBuilder,
      chunkAcc: List[Token])(implicit F: RaiseThrowable[F]): Pull[F, Token, Result[F, Char, List[Token]]] = {
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

    if (idx >= chunk.size)
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((chunk, rest)) => number_(chunk, 0, rest, state, acc, Nil)
        case None =>
          if (NumberState.isFinal(state))
            Pull.output1(Token.NumberValue(acc.result)) >> Pull.pure(None)
          else
            Pull.raiseError[F](new JsonException("unexpected end of input"))
      } else {
      val c = chunk(idx)
      (step(c, state): @switch) match {
        case NumberState.Invalid =>
          if (NumberState.isFinal(state))
            Pull.pure(Some((chunk, idx, rest, Token.NumberValue(acc.result) :: chunkAcc)))
          else
            Pull.raiseError[F](new JsonException(s"invalid number character '$c'"))
        case state =>
          number_(chunk, idx + 1, rest, state, acc.append(c), chunkAcc)
      }
    }
  }

  def keyword_[F[_]](
      chunk: Chunk[Char],
      idx: Int,
      rest: Stream[F, Char],
      expected: String,
      eidx: Int,
      token: Token,
      chunkAcc: List[Token])(implicit F: RaiseThrowable[F]): Pull[F, Token, Result[F, Char, List[Token]]] = {
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((chunk, rest)) => keyword_(chunk, 0, rest, expected, eidx, token, Nil)
        case None                => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      val c = chunk(idx)
      if (c == expected.charAt(eidx)) {
        if (eidx == expected.length - 1)
          Pull.pure(Some((chunk, idx + 1, rest, token :: chunkAcc)))
        else
          keyword_(chunk, idx + 1, rest, expected, eidx + 1, token, chunkAcc)
      } else {
        Pull.raiseError[F](new JsonException(s"unexpected character '$c' (expected $expected)"))
      }
    }
  }

  def value_[F[_]](chunk: Chunk[Char], idx: Int, rest: Stream[F, Char], state: Int, chunkAcc: List[Token])(
      implicit F: RaiseThrowable[F]): Pull[F, Token, Result[F, Char, List[Token]]] =
    if (idx >= chunk.size)
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((chunk, rest)) => value_(chunk, 0, rest, state, Nil)
        case None                => Pull.raiseError[F](new JsonException("unexpected end of input"))
      } else {
      val c = chunk(idx)
      (c: @switch) match {
        case '{' =>
          Pull.suspend(go_(chunk, idx + 1, rest, State.BeforeObjectKey, Nil, Token.StartObject :: chunkAcc))
        case '[' =>
          Pull.suspend(go_(chunk, idx + 1, rest, State.BeforeArrayValue, Nil, Token.StartArray :: chunkAcc))
        case 't' => keyword_(chunk, idx, rest, "true", 0, Token.TrueValue, chunkAcc)
        case 'f' => keyword_(chunk, idx, rest, "false", 0, Token.FalseValue, chunkAcc)
        case 'n' => keyword_(chunk, idx, rest, "null", 0, Token.NullValue, chunkAcc)
        case '"' => string_(chunk, idx + 1, rest, false, StringState.Normal, 0, new StringBuilder, chunkAcc)
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          number_(chunk, idx, rest, NumberState.NumberStart, new StringBuilder, chunkAcc)
        case c => Pull.raiseError[F](new JsonException(s"unexpected '$c'"))
      }
    }

  def continue[F[_]](state: Int, history: List[Int])(result: Result[F, Char, List[Token]])(
      implicit F: RaiseThrowable[F]) =
    result match {
      case Some((chunk, idx, rest, chunkAcc)) =>
        go_(chunk, idx, rest, state, history, chunkAcc)
      case None =>
        if (history.isEmpty)
          Pull.pure(None)
        else
          Pull.raiseError[F](new JsonException("unexpected end of input"))
    }

  def go_[F[_]](chunk: Chunk[Char],
                idx: Int,
                rest: Stream[F, Char],
                state: Int,
                history: List[Int],
                chunkAcc: List[Token])(implicit F: RaiseThrowable[F]): Pull[F, Token, Result[F, Char, List[Token]]] = {
    val idx1 = idx //eatWhitespaces(idx, chunk)
    if (idx1 >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((chunk, rest)) => go_(chunk, 0, rest, state, history, Nil)
        case None =>
          if (history.isEmpty)
            Pull.pure(None)
          else
            Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      val c = chunk(idx1)
      (c: @switch) match {
        case ' ' | '\t' | '\r' | '\n' => go_(chunk: Chunk[Char], idx + 1, rest, state, history, chunkAcc)
        case _ =>
          (state: @switch) match {
            case State.BeforeValue =>
              value_(chunk, idx1, rest, state, chunkAcc).flatMap(continue(State.BeforeValue, history))
            case State.BeforeObjectKey =>
              (c: @switch) match {
                case '"' =>
                  string_(chunk, idx1 + 1, rest, true, StringState.Normal, 0, new StringBuilder, chunkAcc)
                    .flatMap(continue(State.AfterObjectKey, history))
                case '}' =>
                  history match {
                    case prev :: tail => go_(chunk, idx1 + 1, rest, prev, tail, Token.EndObject :: chunkAcc)
                    case Nil          => Pull.pure(Some((chunk, idx1 + 1, rest, Token.EndObject :: chunkAcc)))
                  }
                case _ => Pull.raiseError[F](new JsonException(s"unexpected '$c' before object key"))
              }
            case State.ExpectObjectKey =>
              (c: @switch) match {
                case '"' =>
                  string_(chunk, idx1 + 1, rest, true, StringState.Normal, 0, new StringBuilder, chunkAcc)
                    .flatMap(continue(State.AfterObjectKey, history))
                case _ => Pull.raiseError[F](new JsonException(s"unexpected '$c' before object key"))
              }
            case State.AfterObjectKey =>
              (c: @switch) match {
                case ':' => go_(chunk, idx1 + 1, rest, State.BeforeObjectValue, history, chunkAcc)
                case c   => Pull.raiseError[F](new JsonException(s"unexpected '$c' after object key"))
              }
            case State.BeforeObjectValue =>
              value_(chunk, idx1, rest, State.AfterObjectValue, chunkAcc)
                .flatMap(continue(State.AfterObjectValue, history))
            case State.AfterObjectValue =>
              (c: @switch) match {
                case ',' =>
                  go_(chunk, idx1 + 1, rest, State.ExpectObjectKey, history, chunkAcc)
                case '}' =>
                  history match {
                    case prev :: tail => go_(chunk, idx1 + 1, rest, prev, tail, Token.EndObject :: chunkAcc)
                    case Nil          => Pull.pure(Some((chunk, idx1 + 1, rest, Token.EndObject :: chunkAcc)))
                  }
                case c => Pull.raiseError[F](new JsonException(s"unexpected '$c' after object value"))
              }
            case State.ExpectArrayValue =>
              value_(chunk, idx1, rest, State.AfterArrayValue, chunkAcc)
                .flatMap(continue(State.AfterArrayValue, history))
            case State.BeforeArrayValue =>
              (c: @switch) match {
                case ']' =>
                  history match {
                    case prev :: tail => go_(chunk, idx1 + 1, rest, prev, tail, Token.EndArray :: chunkAcc)
                    case Nil          => Pull.pure(Some((chunk, idx1 + 1, rest, Token.EndArray :: chunkAcc)))
                  }
                case c =>
                  value_(chunk, idx1, rest, State.AfterArrayValue, chunkAcc)
                    .flatMap(continue(State.AfterArrayValue, history))
              }
            case State.AfterArrayValue =>
              (c: @switch) match {
                case ']' =>
                  history match {
                    case prev :: tail => go_(chunk, idx1 + 1, rest, prev, tail, Token.EndArray :: chunkAcc)
                    case Nil          => Pull.pure(Some((chunk, idx1 + 1, rest, Token.EndArray :: chunkAcc)))
                  }
                case ',' =>
                  go_(chunk, idx1 + 1, rest, State.ExpectArrayValue, history, chunkAcc)
                case c => Pull.raiseError[F](new JsonException(s"unexpected '$c' after array value"))
              }
          }
      }
    }
  }

  def pipe[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Char, Token] =
    s => go_(Chunk.empty, 0, s, State.BeforeValue, Nil, Nil).void.stream
}
