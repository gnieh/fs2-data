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

import ast._

import cats._

import scala.collection.compat._
import scala.language.higherKinds

private[json] object ValueParser {

  private def parseChunk[F[_], Json](chunk: Chunk[Token],
                                     idx: Int,
                                     expect: Expect,
                                     stack: List[Elem[Json]],
                                     chunkAcc: List[Json])(
      implicit F: ApplicativeError[F, Throwable],
      builder: Builder[Json]): Pull[F, Json, (Expect, List[Elem[Json]])] = {
    val (expect1, stack1, chunk1) =
      chunk.foldLeft((expect, stack, List.empty[Json])) {
        // single value
        case ((Expect.Value, stack, chunkAcc), Token.StartObject) =>
          (Expect.ObjectKey, Elem.StartObject[Json](Expect.Value) :: stack, chunkAcc)
        case ((Expect.Value, stack, chunkAcc), Token.StartArray) =>
          (Expect.ArrayValue, Elem.StartArray[Json](Expect.Value) :: stack, chunkAcc)
        case ((Expect.Value, stack, chunkAcc), Token.FalseValue) =>
          val (stack1, chunkAcc1) =
            if (stack.isEmpty)
              (stack, builder.makeFalse :: chunkAcc)
            else
              (Elem.Value(builder.makeFalse) :: stack, chunkAcc)
          (Expect.Value, stack1, chunkAcc1)
        case ((Expect.Value, stack, chunkAcc), Token.TrueValue) =>
          val (stack1, chunkAcc1) =
            if (stack.isEmpty)
              (stack, builder.makeTrue :: chunkAcc)
            else
              (Elem.Value(builder.makeTrue) :: stack, chunkAcc)
          (Expect.Value, stack1, chunkAcc1)
        case ((Expect.Value, stack, chunkAcc), Token.NullValue) =>
          val (stack1, chunkAcc1) =
            if (stack.isEmpty)
              (stack, builder.makeNull :: chunkAcc)
            else
              (Elem.Value(builder.makeNull) :: stack, chunkAcc)
          (Expect.Value, stack1, chunkAcc1)
        case ((Expect.Value, stack, chunkAcc), Token.StringValue(s)) =>
          val (stack1, chunkAcc1) =
            if (stack.isEmpty)
              (stack, builder.makeString(s) :: chunkAcc)
            else
              (Elem.Value(builder.makeString(s)) :: stack, chunkAcc)
          (Expect.Value, stack1, chunkAcc1)
        case ((Expect.Value, stack, chunkAcc), Token.NumberValue(s)) =>
          val (stack1, chunkAcc1) =
            if (stack.isEmpty)
              (stack, builder.makeNumber(s) :: chunkAcc)
            else
              (Elem.Value(builder.makeNumber(s)) :: stack, chunkAcc)
          (Expect.Value, stack1, chunkAcc1)
        // object fields
        case ((Expect.ObjectKey, stack, chunkAcc), Token.Key(k)) =>
          (Expect.ObjectValue, Elem.Key[Json](k) :: stack, chunkAcc)
        case ((Expect.ObjectKey, stack, chunkAcc), Token.EndObject) =>
          // pop values until corresponding opening object
          val (fields, Elem.StartObject(prev) :: stack1) = stack.span {
            case Elem.StartObject(_) => false
            case _                   => true
          }
          val fields1 = fields.sliding(2, 2).map {
            case List(Elem.Value(json), Elem.Key(key)) => (key, json)
            case _                                     => throw new JsonException("malformed json")
          }
          val obj = builder.makeObject(fields1.toList.reverse)
          val (stack2, chunkAcc1) =
            if (stack1.isEmpty)
              (stack1, obj :: chunkAcc)
            else
              (Elem.Value(obj) :: stack1, chunkAcc)
          (prev, stack2, chunkAcc1)
        case ((Expect.ObjectValue, stack, chunkAcc), Token.StartObject) =>
          (Expect.ObjectKey, Elem.StartObject[Json](Expect.ObjectKey) :: stack, chunkAcc)
        case ((Expect.ObjectValue, stack, chunkAcc), Token.StartArray) =>
          (Expect.ArrayValue, Elem.StartArray[Json](Expect.ObjectKey) :: stack, chunkAcc)
        case ((Expect.ObjectValue, stack, chunkAcc), Token.FalseValue) =>
          (Expect.ObjectKey, Elem.Value(builder.makeFalse) :: stack, chunkAcc)
        case ((Expect.ObjectValue, stack, chunkAcc), Token.TrueValue) =>
          (Expect.ObjectKey, Elem.Value(builder.makeTrue) :: stack, chunkAcc)
        case ((Expect.ObjectValue, stack, chunkAcc), Token.NullValue) =>
          (Expect.ObjectKey, Elem.Value(builder.makeNull) :: stack, chunkAcc)
        case ((Expect.ObjectValue, stack, chunkAcc), Token.StringValue(s)) =>
          (Expect.ObjectKey, Elem.Value(builder.makeString(s)) :: stack, chunkAcc)
        case ((Expect.ObjectValue, stack, chunkAcc), Token.NumberValue(s)) =>
          (Expect.ObjectKey, Elem.Value(builder.makeNumber(s)) :: stack, chunkAcc)
        // array values
        case ((Expect.ArrayValue, stack, chunkAcc), Token.EndArray) =>
          // pop values until corresponding opening array
          val (values, Elem.StartArray(prev) :: stack1) = stack.span {
            case Elem.StartArray(_) => false
            case _                  => true
          }
          val array = builder.makeArray(
            values.reverseIterator
              .map {
                case Elem.Value(json) => json
                case _                => throw new JsonException("malformed json")
              }
              .to(Iterable))
          val (stack2, chunkAcc1) =
            if (stack1.isEmpty)
              (stack1, array :: chunkAcc)
            else
              (Elem.Value(array) :: stack1, chunkAcc)
          (prev, stack2, chunkAcc1)
        case ((Expect.ArrayValue, stack, chunkAcc), Token.StartObject) =>
          (Expect.ObjectKey, Elem.StartObject[Json](Expect.ArrayValue) :: stack, chunkAcc)
        case ((Expect.ArrayValue, stack, chunkAcc), Token.StartArray) =>
          (Expect.ArrayValue, Elem.StartArray[Json](Expect.ArrayValue) :: stack, chunkAcc)
        case ((Expect.ArrayValue, stack, chunkAcc), Token.FalseValue) =>
          (Expect.ArrayValue, Elem.Value(builder.makeFalse) :: stack, chunkAcc)
        case ((Expect.ArrayValue, stack, chunkAcc), Token.TrueValue) =>
          (Expect.ArrayValue, Elem.Value(builder.makeTrue) :: stack, chunkAcc)
        case ((Expect.ArrayValue, stack, chunkAcc), Token.NullValue) =>
          (Expect.ArrayValue, Elem.Value(builder.makeNull) :: stack, chunkAcc)
        case ((Expect.ArrayValue, stack, chunkAcc), Token.StringValue(s)) =>
          (Expect.ArrayValue, Elem.Value(builder.makeString(s)) :: stack, chunkAcc)
        case ((Expect.ArrayValue, stack, chunkAcc), Token.NumberValue(s)) =>
          (Expect.ArrayValue, Elem.Value(builder.makeNumber(s)) :: stack, chunkAcc)
        // errors
        case ((expect, stack, chunkAcc), token) =>
          throw new JsonException(s"malformed json (state: $expect, stack: $stack, token: $token)")
      }
    Pull.output(Chunk.seq(chunk1.reverse)) >> Pull.pure((expect1, stack1))
  }

  def go[F[_], Json](s: Stream[F, Token], expect: Expect, stack: List[Elem[Json]])(
      implicit F: ApplicativeError[F, Throwable],
      builder: Builder[Json]): Pull[F, Json, Unit] =
    s.pull.uncons.flatMap {
      case Some((hd, tl)) =>
        parseChunk(hd, 0, expect, stack, Nil).flatMap {
          case (expect, stack) => go(tl, expect, stack)
        }
      case None =>
        if (stack.isEmpty)
          Pull.done
        else
          Pull.raiseError[F](new JsonException("unexpected end of input"))
    }

  def pipe[F[_], Json](implicit F: ApplicativeError[F, Throwable], builder: Builder[Json]): Pipe[F, Token, Json] =
    s => go(s, Expect.Value, Nil).stream

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
