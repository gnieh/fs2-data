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

import cats._
import cats.data.NonEmptyList
import cats.implicits._

import scala.annotation.switch
import scala.collection.compat._
import scala.language.higherKinds

/** Handles stream parsing and traversing of json documents.
  */
package object json {

  private val hexa = "0123456789abcdefABCDEF"

  /** Transforms a stream of characters into a stream of Json tokens.
    * Emitted tokens are guaranteed to be valid up to that point.
    * If the streams ends without failure, the sequence of tokens is sensured
    * to represent a (potentially empty) sequence of valid Json documents.
    */
  def tokens[F[_]](implicit F: MonadError[F, Throwable]): Pipe[F, Char, Token] = {
    // the opening quote has already been read
    def string(s: Stream[F, Char], key: Boolean): Pull[F, Token, Stream[F, Char]] = {
      def loop(s: Stream[F, Char], acc: StringBuilder): Pull[F, Token, Stream[F, Char]] =
        s.pull.uncons1.flatMap {
          case Some(('"', rest)) =>
            // end of string
            Pull.output1(if (key) Token.Key(acc.result) else Token.StringValue(acc.result)) >> Pull.pure(rest)
          case Some(('\\', rest)) =>
            // escaped character
            rest.pull.uncons1.flatMap {
              case Some(('"', rest))  => loop(rest, acc.append('"'))
              case Some(('\\', rest)) => loop(rest, acc.append('\\'))
              case Some(('/', rest))  => loop(rest, acc.append('/'))
              case Some(('b', rest))  => loop(rest, acc.append('\b'))
              case Some(('f', rest))  => loop(rest, acc.append('\f'))
              case Some(('n', rest))  => loop(rest, acc.append('\n'))
              case Some(('r', rest))  => loop(rest, acc.append('\r'))
              case Some(('t', rest))  => loop(rest, acc.append('\t'))
              case Some(('u', rest)) =>
                rest.pull.unconsN(4, false).flatMap {
                  case Some((chunk, rest)) if chunk.forall(hexa.contains(_)) =>
                    loop(rest, acc.appendAll(Character.toChars(Integer.parseInt(chunk.mkString_(""), 16))))
                  case Some((chunk, _)) =>
                    Pull.raiseError[F](
                      new JsonException(s"malformed escaped unicode sequence ${chunk.mkString_("\\u", "", "")}"))
                  case None => Pull.raiseError[F](new JsonException("unexpected end of input"))
                }
              case Some((c, _)) => Pull.raiseError[F](new JsonException(s"unknown escaped character '$c'"))
              case None         => Pull.raiseError[F](new JsonException("unexpected end of input"))
            }
          case Some((c, rest)) if c >= 0x20 && c <= 0x10ffff =>
            loop(rest, acc.append(c))
          case Some((c, _)) =>
            Pull.raiseError[F](new JsonException(s"invalid string character '$c'"))
        }
      loop(s, new StringBuilder)
    }

    def number(fst: Char, rest: Stream[F, Char]): Pull[F, Token, Stream[F, Char]] = {
      def step(c: Char, state: NumberState): NumberState =
        (c, state) match {
          case ('-', NumberState.NumberStart) =>
            NumberState.IntegerStart
          case ('0', NumberState.NumberStart | NumberState.IntegerStart) =>
            NumberState.FractionStart
          case ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9',
                NumberState.NumberStart | NumberState.IntegerStart) =>
            NumberState.IntegerBody
          case ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9',
                NumberState.IntegerBody | NumberState.FractionBody | NumberState.ExponentBody) =>
            state
          case ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9', NumberState.FractionOne) =>
            NumberState.FractionBody
          case ('.', NumberState.IntegerBody | NumberState.FractionStart) =>
            NumberState.FractionOne
          case ('e' | 'E', NumberState.IntegerBody | NumberState.FractionStart | NumberState.FractionBody) =>
            NumberState.ExponentSign
          case ('-' | '+', NumberState.ExponentSign) =>
            NumberState.ExponentOne
          case ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9',
                NumberState.ExponentSign | NumberState.ExponentOne) =>
            NumberState.ExponentBody
          case (c, _) =>
            NumberState.Invalid
        }

      def loop(s: Stream[F, Char], state: NumberState, acc: StringBuilder): Pull[F, Token, Stream[F, Char]] =
        s.pull.peek1.flatMap {
          case Some((c, rest)) =>
            step(c, state) match {
              case NumberState.Invalid =>
                if (state.isFinal)
                  Pull.output1(Token.NumberValue(acc.result)) >> Pull.pure(rest)
                else
                  Pull.raiseError[F](new JsonException(s"invalid number character '$c'"))
              case state =>
                acc.append(c)
                loop(rest.tail, state, acc)
            }
          case None =>
            if (state.isFinal)
              Pull.output1(Token.NumberValue(acc.result)) >> Pull.pure(Stream.empty)
            else
              Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
      val builder = new StringBuilder
      builder.append(fst)
      loop(rest, step(fst, NumberState.NumberStart), builder)
    }

    def keyword(fst: Char, value: String, token: Token, s: Stream[F, Char]): Pull[F, Token, Stream[F, Char]] = {
      def loop(s: Stream[F, Char], acc: StringBuilder): Pull[F, Token, Stream[F, Char]] =
        s.pull.peek1.flatMap {
          case Some((c, rest)) if c.isLetter => loop(rest.tail, acc.append(c))
          case Some((c, rest)) =>
            if (acc.result == value)
              Pull.output1(token) >> Pull.pure(rest)
            else
              Pull.raiseError[F](new JsonException(s"unknown keyword '$fst${acc.result}' (expected $fst$value)"))
          case None =>
            if (acc.result == value)
              Pull.output1(token) >> Pull.pure(Stream.empty)
            else
              Pull.raiseError[F](new JsonException("unexpected end of input"))
        }
      loop(s, new StringBuilder)
    }

    def value(fst: Char, rest: Stream[F, Char], state: State): Pull[F, Token, Stream[F, Char]] =
      (fst: @switch) match {
        case '{'                                                             => Pull.output1(Token.StartObject) >> go(rest, State.BeforeObjectKey, Nil)
        case '['                                                             => Pull.output1(Token.StartArray) >> go(rest, State.BeforeArrayValue, Nil)
        case 't'                                                             => keyword(fst, "rue", Token.TrueValue, rest)
        case 'f'                                                             => keyword(fst, "alse", Token.FalseValue, rest)
        case 'n'                                                             => keyword(fst, "ull", Token.NullValue, rest)
        case '"'                                                             => string(rest, false)
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => number(fst, rest)
        case c                                                               => Pull.raiseError[F](new JsonException(s"unexpected '$c'"))
      }

    def isWhitespace(c: Char): Boolean =
      "\u0020\u000d\u000a\u0009".contains(c)

    def go(s: Stream[F, Char], state: State, history: List[State]): Pull[F, Token, Stream[F, Char]] =
      s.dropWhile(isWhitespace(_)).pull.uncons1.flatMap {
        case Some((c, rest)) =>
          state match {
            case State.BeforeValue =>
              value(c, rest, state).flatMap(go(_, State.BeforeValue, history))
            case State.BeforeObjectKey =>
              (c: @switch) match {
                case '"' => string(rest, true).flatMap(go(_, State.AfterObjectKey, history))
                case '}' =>
                  Pull.output1(Token.EndObject) >>
                    (history match {
                      case prev :: tail => go(rest, prev, tail)
                      case Nil          => Pull.pure(rest)
                    })
                case c => Pull.raiseError[F](new JsonException(s"unexpected '$c' before object key"))
              }
            case State.ExpectObjectKey =>
              (c: @switch) match {
                case '"' => string(rest, true).flatMap(go(_, State.AfterObjectKey, history))
                case c   => Pull.raiseError[F](new JsonException(s"unexpected '$c' before object key"))
              }
            case State.AfterObjectKey =>
              (c: @switch) match {
                case ':' => go(rest, State.BeforeObjectValue, history)
                case c   => Pull.raiseError[F](new JsonException(s"unexpected '$c' after object key"))
              }
            case State.BeforeObjectValue =>
              value(c, rest, State.AfterObjectValue).flatMap(go(_, State.AfterObjectValue, history))
            case State.AfterObjectValue =>
              (c: @switch) match {
                case ',' =>
                  go(rest, State.ExpectObjectKey, history)
                case '}' =>
                  Pull.output1(Token.EndObject) >>
                    (history match {
                      case prev :: tail => go(rest, prev, tail)
                      case Nil          => Pull.pure(rest)
                    })
                case c => Pull.raiseError[F](new JsonException(s"unexpected '$c' after object value"))
              }
            case State.ExpectArrayValue =>
              value(c, rest, State.AfterArrayValue).flatMap(go(_, State.AfterArrayValue, history))
            case State.BeforeArrayValue =>
              (c: @switch) match {
                case ']' =>
                  Pull.output1(Token.EndArray) >>
                    (history match {
                      case prev :: tail => go(rest, prev, tail)
                      case Nil          => Pull.pure(rest)
                    })
                case c => value(c, rest, State.AfterArrayValue).flatMap(go(_, State.AfterArrayValue, history))
              }
            case State.AfterArrayValue =>
              (c: @switch) match {
                case ']' =>
                  Pull.output1(Token.EndArray) >>
                    (history match {
                      case prev :: tail => go(rest, prev, tail)
                      case Nil          => Pull.pure(rest)
                    })
                case ',' =>
                  go(rest, State.ExpectArrayValue, history)
                case c => Pull.raiseError[F](new JsonException(s"unexpected '$c' after array value"))
              }
          }
        case None =>
          if (history.isEmpty)
            Pull.pure(Stream.empty)
          else
            Pull.raiseError[F](new JsonException("unexpected end of input"))
      }

    s => go(s, State.BeforeValue, Nil).stream
  }

  private sealed trait Expect
  private object Expect {
    case object Value extends Expect
    case object ArrayValue extends Expect
    case object ObjectValue extends Expect
    case object ObjectKey extends Expect
  }

  private sealed trait Elem[Json]
  private object Elem {
    case class StartObject[Json](prev: Expect) extends Elem[Json]
    case class StartArray[Json](prev: Expect) extends Elem[Json]
    case class Value[Json](json: Json) extends Elem[Json]
    case class Key[Json](name: String) extends Elem[Json]
  }

  private object OnlyJsons {
    def unapply[Json](stack: List[Elem[Json]]): Option[List[Json]] = {
      val jsons =
        stack.collect {
          case Elem.Value(json) => json
        }
      if (jsons.size == stack.size)
        Some(jsons)
      else
        None
    }
  }

  implicit class CompileCirceOps[F[_], G[_]](val cops: Stream.CompileOps[F, G, Token]) extends AnyVal {

    /** Given a `Json` AST type and an AST builder for that type, accumulate the
      * tokens to create a value of that json. */
    def toJson[Json](implicit G: MonadError[G, Throwable], builder: ast.Builder[Json]): G[Json] =
      toJsonList[Json].flatMap {
        case List(json) => G.pure(json)
        case l          => G.raiseError(new JsonException(s"exactly one json value was expected (but got ${l.size})"))
      }

    /** Given a `Json` AST type and an AST builder for that type, accumulate the
      * tokens to create an value of that json. */
    def toJsonOpt[Json](implicit G: MonadError[G, Throwable], builder: ast.Builder[Json]): G[Option[Json]] =
      toJsonList[Json].flatMap {
        case Nil        => G.pure(None)
        case List(json) => G.pure(Some(json))
        case l          => G.raiseError(new JsonException(s"zero or one json value was expected (but got ${l.size})"))
      }

    /** Given a `Json` AST type and an AST builder for that type, accumulate the
      * tokens to create a potentially empty list of json values. */
    def toJsonList[Json](implicit G: MonadError[G, Throwable], builder: ast.Builder[Json]): G[List[Json]] =
      cops
        .fold((Expect.Value: Expect, List.empty[Elem[Json]])) {
          // single value
          case ((Expect.Value, stack), Token.StartObject) =>
            (Expect.ObjectKey, Elem.StartObject[Json](Expect.Value) :: stack)
          case ((Expect.Value, stack), Token.StartArray) =>
            (Expect.ArrayValue, Elem.StartArray[Json](Expect.Value) :: stack)
          case ((Expect.Value, stack), Token.FalseValue) =>
            (Expect.Value, Elem.Value(builder.makeFalse) :: stack)
          case ((Expect.Value, stack), Token.TrueValue) =>
            (Expect.Value, Elem.Value(builder.makeTrue) :: stack)
          case ((Expect.Value, stack), Token.NullValue) =>
            (Expect.Value, Elem.Value(builder.makeNull) :: stack)
          case ((Expect.Value, stack), Token.StringValue(s)) =>
            (Expect.Value, Elem.Value(builder.makeString(s)) :: stack)
          case ((Expect.Value, stack), Token.NumberValue(s)) =>
            (Expect.Value, Elem.Value(builder.makeNumber(s)) :: stack)
          // object fields
          case ((Expect.ObjectKey, stack), Token.Key(k)) =>
            (Expect.ObjectValue, Elem.Key[Json](k) :: stack)
          case ((Expect.ObjectKey, stack), Token.EndObject) =>
            // pop values until corresponding opening object
            val (fields, Elem.StartObject(prev) :: stack1) = stack.span {
              case Elem.StartObject(_) => false
              case _                   => true
            }
            val fields1 = fields.sliding(2, 2).map {
              case List(Elem.Value(json), Elem.Key(key)) => (key, json)
              case _                                     => throw new JsonException("malformed json")
            }
            (prev, Elem.Value(builder.makeObject(fields1.toList.reverse)) :: stack1)
          case ((Expect.ObjectValue, stack), Token.StartObject) =>
            (Expect.ObjectKey, Elem.StartObject[Json](Expect.ObjectKey) :: stack)
          case ((Expect.ObjectValue, stack), Token.StartArray) =>
            (Expect.ArrayValue, Elem.StartArray[Json](Expect.ObjectKey) :: stack)
          case ((Expect.ObjectValue, stack), Token.FalseValue) =>
            (Expect.ObjectKey, Elem.Value(builder.makeFalse) :: stack)
          case ((Expect.ObjectValue, stack), Token.TrueValue) =>
            (Expect.ObjectKey, Elem.Value(builder.makeTrue) :: stack)
          case ((Expect.ObjectValue, stack), Token.NullValue) =>
            (Expect.ObjectKey, Elem.Value(builder.makeNull) :: stack)
          case ((Expect.ObjectValue, stack), Token.StringValue(s)) =>
            (Expect.ObjectKey, Elem.Value(builder.makeString(s)) :: stack)
          case ((Expect.ObjectValue, stack), Token.NumberValue(s)) =>
            (Expect.ObjectKey, Elem.Value(builder.makeNumber(s)) :: stack)
          // array values
          case ((Expect.ArrayValue, stack), Token.EndArray) =>
            // pop values until corresponding opening array
            val (values, Elem.StartArray(prev) :: stack1) = stack.span {
              case Elem.StartArray(_) => false
              case _                  => true
            }
            (prev,
             Elem.Value(builder.makeArray(values.reverseIterator
               .map {
                 case Elem.Value(json) => json
                 case _                => throw new JsonException("malformed json")
               }
               .to(Iterable))) :: stack1)
          case ((Expect.ArrayValue, stack), Token.StartObject) =>
            (Expect.ObjectKey, Elem.StartObject[Json](Expect.ArrayValue) :: stack)
          case ((Expect.ArrayValue, stack), Token.StartArray) =>
            (Expect.ArrayValue, Elem.StartArray[Json](Expect.ArrayValue) :: stack)
          case ((Expect.ArrayValue, stack), Token.FalseValue) =>
            (Expect.ArrayValue, Elem.Value(builder.makeFalse) :: stack)
          case ((Expect.ArrayValue, stack), Token.TrueValue) =>
            (Expect.ArrayValue, Elem.Value(builder.makeTrue) :: stack)
          case ((Expect.ArrayValue, stack), Token.NullValue) =>
            (Expect.ArrayValue, Elem.Value(builder.makeNull) :: stack)
          case ((Expect.ArrayValue, stack), Token.StringValue(s)) =>
            (Expect.ArrayValue, Elem.Value(builder.makeString(s)) :: stack)
          case ((Expect.ArrayValue, stack), Token.NumberValue(s)) =>
            (Expect.ArrayValue, Elem.Value(builder.makeNumber(s)) :: stack)
          // errors
          case ((expect, stack), token) =>
            throw new JsonException(s"malformed json (state: $expect, stack: $stack, token: $token)")
        }
        .flatMap {
          case (Expect.Value, OnlyJsons(jsons)) => G.pure(jsons)
          case (expect, stack)                  => G.raiseError(new JsonException("malformed json"))
        }

  }

}
