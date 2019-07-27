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

import json.ast._
import json.internals._

import cats._
import cats.data.NonEmptyList
import cats.implicits._

import scala.annotation.switch
import scala.collection.compat._
import scala.language.higherKinds

/** Handles stream parsing and traversing of json documents.
  */
package object json {

  /** Transforms a stream of characters into a stream of Json tokens.
    * Emitted tokens are guaranteed to be valid up to that point.
    * If the streams ends without failure, the sequence of tokens is sensured
    * to represent a (potentially empty) sequence of valid Json documents.
    */
  def tokens[F[_]](implicit F: ApplicativeError[F, Throwable]): Pipe[F, Char, Token] =
    TokenParser.pipe[F]

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
