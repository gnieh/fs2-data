package fs2
package data
package json
package internals

import fs2.data.text.AsCharBuffer

import scala.annotation.switch
import scala.collection.immutable.VectorBuilder

import TokenParser._

private class JsonTokenParser[F[_], T](s: Stream[F, T])(implicit F: RaiseThrowable[F], T: AsCharBuffer[F, T]) {
  private[this] var context = T.create(s)
  private[this] final val chunkAcc = new VectorBuilder[Token]

  // the opening quote has already been read
  private final def string_(key: Boolean, acc: StringBuilder): Pull[F, Token, Unit] =
    if (T.needsPull(context)) {
      T.appendMarked(context, acc)
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          this.context = context
          chunkAcc.clear()
          string_(key, acc)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      (T.current(context): @switch) match {
        case '"' =>
          T.appendMarked(context, acc)
          val res = acc.result()
          val token = if (key) Token.Key(res) else Token.StringValue(res)
          T.advance(context)
          chunkAcc += token
          Pull.done
        case '\\' =>
          T.appendMarked(context, acc)
          T.advance(context)
          slowString_(key, StringState.SeenBackslash, 0, acc)
        case c if c >= 0x20 && c <= 0x10ffff =>
          T.advance(context)
          string_(key, acc)
        case c =>
          emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"invalid string character '$c'"))
      }
    }

  private final def slowString_(key: Boolean, state: Int, unicode: Int, acc: StringBuilder): Pull[F, Token, Unit] = {
    if (T.needsPull(context)) {
      T.appendMarked(context, acc)
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          this.context = context
          chunkAcc.clear()
          slowString_(key, state, unicode, acc)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      val c = T.current(context)
      (state: @switch) match {
        case StringState.SeenBackslash =>
          T.advance(context)
          T.mark(context)
          (c: @switch) match {
            case '"'  => slowString_(key, StringState.Normal, 0, acc.append('"'))
            case '\\' => slowString_(key, StringState.Normal, 0, acc.append('\\'))
            case '/'  => slowString_(key, StringState.Normal, 0, acc.append('/'))
            case 'b'  => slowString_(key, StringState.Normal, 0, acc.append('\b'))
            case 'f'  => slowString_(key, StringState.Normal, 0, acc.append('\f'))
            case 'n'  => slowString_(key, StringState.Normal, 0, acc.append('\n'))
            case 'r'  => slowString_(key, StringState.Normal, 0, acc.append('\r'))
            case 't'  => slowString_(key, StringState.Normal, 0, acc.append('\t'))
            case 'u'  => slowString_(key, StringState.Expect4Unicode, 0, acc)
            case _ =>
              emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unknown escaped character '$c'"))
          }
        case StringState.Normal =>
          if (c == '"') {
            T.appendMarked(context, acc)
            val res = acc.result()
            val token = if (key) Token.Key(res) else Token.StringValue(res)
            T.advance(context)
            chunkAcc += token
            Pull.done
          } else if (c == '\\') {
            T.appendMarked(context, acc)
            T.advance(context)
            slowString_(key, StringState.SeenBackslash, 0, acc)
          } else if (c >= 0x20 && c <= 0x10ffff) {
            T.advance(context)
            slowString_(key, StringState.Normal, 0, acc)
          } else
            emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"invalid string character '$c'"))
        case n /* StringState.ExpectNUnicode */ =>
          val cidx = hexa.indexOf(c.toLower.toInt)
          if (cidx >= 0) {
            val unicode1 = (unicode << 4) | (0x0000000f & cidx)
            if (n == 1) {
              T.advance(context)
              T.mark(context)
              slowString_(key, StringState.Normal, 0, acc.appendAll(Character.toChars(unicode1)))
            } else {
              T.advance(context)
              slowString_(key, n - 1, unicode1, acc)
            }
          } else {
            emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException("malformed escaped unicode sequence"))
          }
      }
    }
  }

  private final def number_(state: Int, acc: StringBuilder): Pull[F, Token, Unit] = {
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
      T.appendMarked(context, acc)
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          this.context = context
          chunkAcc.clear()
          number_(state, acc)
        case None =>
          this.context = T.create(Stream.empty)
          chunkAcc.clear()
          if (NumberState.isFinal(state))
            Pull.output1(Token.NumberValue(acc.result()))
          else
            Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      val c = T.current(context)
      (step(c, state): @switch) match {
        case NumberState.Invalid =>
          if (NumberState.isFinal(state)) {
            T.appendMarked(context, acc)
            chunkAcc += Token.NumberValue(acc.result())
            Pull.done
          } else
            emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"invalid number character '$c'"))
        case state =>
          T.advance(context)
          number_(state, acc)
      }
    }
  }

  private final def keyword_(expected: String, eidx: Int, elen: Int, token: Token): Pull[F, Token, Unit] = {
    if (T.needsPull(context)) {
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          this.context = context
          chunkAcc.clear()
          keyword_(expected, eidx, elen, token)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      val c = T.current(context)
      if (c == expected.charAt(eidx)) {
        if (eidx == elen - 1) {
          T.advance(context)
          chunkAcc += token
          Pull.done
        } else {
          T.advance(context)
          keyword_(expected, eidx + 1, elen, token)
        }
      } else {
        emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected character '$c' (expected $expected)"))
      }
    }
  }

  private final def value_(state: Int)(implicit F: RaiseThrowable[F]): Pull[F, Token, Unit] =
    if (T.needsPull(context)) {
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          this.context = context
          chunkAcc.clear()
          value_(state)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
      }
    } else {
      val c = T.current(context)
      (c: @switch) match {
        case '{' =>
          T.advance(context)
          chunkAcc += Token.StartObject
          Pull.suspend(go_(State.BeforeObjectKey))
        case '[' =>
          T.advance(context)
          chunkAcc += Token.StartArray
          Pull.suspend(go_(State.BeforeArrayValue))
        case 't' => keyword_("true", 0, 4, Token.TrueValue)
        case 'f' => keyword_("false", 0, 5, Token.FalseValue)
        case 'n' => keyword_("null", 0, 4, Token.NullValue)
        case '"' =>
          T.advance(context)
          T.mark(context)
          string_(false, new StringBuilder(stringBufferCapacity))
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          T.mark(context)
          number_(NumberState.NumberStart, new StringBuilder(numberBufferCapacity))
        case _ => emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c'"))
      }
    }

  final def go_(state: Int): Pull[F, Token, Unit] = {
    if (T.needsPull(context)) {
      emitChunk(chunkAcc) >> T.pullNext(context).flatMap {
        case Some(context) =>
          this.context = context
          chunkAcc.clear()
          go_(state)
        case None =>
          this.context = T.create(Stream.empty)
          chunkAcc.clear()
          Pull.done
      }
    } else {
      val c = T.current(context)
      (c: @switch) match {
        case ' ' | '\t' | '\r' | '\n' =>
          T.advance(context)
          go_(state)
        case _ =>
          (state: @switch) match {
            case State.BeforeValue =>
              value_(state) >> go_(State.BeforeValue)
            case State.BeforeObjectKey =>
              (c: @switch) match {
                case '"' =>
                  T.advance(context)
                  T.mark(context)
                  string_(true, new StringBuilder(keyBufferCapacity)) >> go_(State.AfterObjectKey)
                case '}' =>
                  T.advance(context)
                  chunkAcc += Token.EndObject
                  Pull.done
                case _ =>
                  emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' before object key"))
              }
            case State.ExpectObjectKey =>
              (c: @switch) match {
                case '"' =>
                  T.advance(context)
                  T.mark(context)
                  string_(true, new StringBuilder(keyBufferCapacity)) >> go_(State.AfterObjectKey)
                case _ =>
                  emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' before object key"))
              }
            case State.AfterObjectKey =>
              (c: @switch) match {
                case ':' =>
                  T.advance(context)
                  go_(State.BeforeObjectValue)
                case c =>
                  emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' after object key"))
              }
            case State.BeforeObjectValue =>
              value_(State.AfterObjectValue) >> go_(State.AfterObjectValue)
            case State.AfterObjectValue =>
              (c: @switch) match {
                case ',' =>
                  T.advance(context)
                  go_(State.ExpectObjectKey)
                case '}' =>
                  T.advance(context)
                  chunkAcc += Token.EndObject
                  Pull.done
                case c =>
                  emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' after object value"))
              }
            case State.ExpectArrayValue =>
              value_(State.AfterArrayValue) >> go_(State.AfterArrayValue)
            case State.BeforeArrayValue =>
              (c: @switch) match {
                case ']' =>
                  T.advance(context)
                  chunkAcc += Token.EndArray
                  Pull.done
                case _ =>
                  value_(State.AfterArrayValue) >> go_(State.AfterArrayValue)
              }
            case State.AfterArrayValue =>
              (c: @switch) match {
                case ']' =>
                  T.advance(context)
                  chunkAcc += Token.EndArray
                  Pull.done
                case ',' =>
                  T.advance(context)
                  go_(State.ExpectArrayValue)
                case c =>
                  emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"unexpected '$c' after array value"))
              }
          }
      }
    }
  }

}
