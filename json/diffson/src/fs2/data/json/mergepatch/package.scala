/*
 * Copyright 2020 Lucas Satabin
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

import ast._
import internals._

import cats._

import diffson._
import diffson.jsonmergepatch._

package object mergepatch {

  // opening brace has been consumed
  private def patchObject[F[_], Json](chunk: Chunk[Token],
                                      idx: Int,
                                      rest: Stream[F, Token],
                                      patch: Map[String, Json],
                                      chunkAcc: List[Token])(
      implicit F: ApplicativeError[F, Throwable],
      Json: Jsony[Json],
      tokenizer: Tokenizer[Json]): Pull[F, Token, Result[F, Token, List[Token]]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) => patchObject(hd, 0, tl, patch, Nil)
        case None           => Pull.pure(None)
      }
    } else {
      chunk(idx) match {
        case token @ Token.Key(key) =>
          patch.get(key) match {
            case Some(JsObject(fields)) =>
              // key was found, recursively patch value
              patchChunk(chunk, idx + 1, rest, JsonMergePatch.Object(fields), token :: chunkAcc).flatMap {
                case Some((chunk, idx, rest, chunkAcc)) =>
                  patchObject(chunk, idx, rest, patch - key, chunkAcc)
                case None =>
                  // this is really malformed and should have been caught before
                  // anyway, just raise the error
                  Pull.raiseError[F](new JsonException("malformed json"))
              }
            case Some(value) =>
              // the patch is a value, skip current json value and replace or delete
              skipValue(chunk, idx + 1, rest, 0, chunkAcc).flatMap {
                case Some((chunk, idx, rest, chunkAcc)) =>
                  if (value == Json.Null)
                    // setting null means deleting the key, just continue
                    patchObject(chunk, idx, rest, patch - key, chunkAcc)
                  else
                    // replace current value by the patch one
                    patchObject(chunk,
                                idx,
                                rest,
                                patch - key,
                                tokenizer.tokenize(value).toList reverse_::: (token :: chunkAcc))
                case None =>
                  // this is really malformed and should have been caught before
                  // anyway, just raise the error
                  Pull.raiseError[F](new JsonException("malformed json"))
              }
            case None =>
              // this object key is not in the patch, just emit it unchanged
              emitValue(chunk, idx + 1, rest, 0, token :: chunkAcc).flatMap {
                case Some((chunk, idx, rest, chunkAcc)) =>
                  patchObject(chunk, idx, rest, patch, chunkAcc)
                case None =>
                  // this is really malformed and should have been caught before
                  // anyway, just raise the error
                  Pull.raiseError[F](new JsonException("malformed json"))
              }
          }
        case Token.EndObject =>
          // object is done, add all patch key/values that were not found in the patched object (if any)
          if (patch.isEmpty)
            Pull.pure(Some((chunk, idx + 1, rest, Token.EndObject :: chunkAcc)))
          else
            Pull.pure(Some((chunk, idx + 1, rest, Token.EndObject :: (patch.flatMap {
              case (key, value) => Token.Key(key) :: tokenizer.tokenize(value).toList
            }.toList reverse_::: chunkAcc))))
        case _ =>
          // this is really malformed and should have been caught before
          // anyway, just raise the error
          Pull.raiseError[F](new JsonException("malformed json"))
      }
    }

  private def patchChunk[F[_], Json](chunk: Chunk[Token],
                                     idx: Int,
                                     rest: Stream[F, Token],
                                     patch: JsonMergePatch[Json],
                                     chunkAcc: List[Token])(
      implicit F: ApplicativeError[F, Throwable],
      Json: Jsony[Json],
      tokenizer: Tokenizer[Json]): Pull[F, Token, Result[F, Token, List[Token]]] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.seq(chunkAcc.reverse)) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) => patchChunk(hd, 0, tl, patch, Nil)
        case None           => Pull.pure(None)
      }
    } else {
      patch match {
        case JsonMergePatch.Object(fields) =>
          chunk(idx) match {
            case Token.StartObject =>
              // we are patching an object, go through the object and patch recursively
              patchObject(chunk, idx + 1, rest, fields, Token.StartObject :: chunkAcc)
            case Token.StartArray | Token.NullValue | Token.TrueValue | Token.FalseValue | Token.NumberValue(_) |
                Token.StringValue(_) =>
              // applying a patch to a non object simply replaces the value with the object
              skipValue(chunk, idx, rest, 0, chunkAcc).flatMap { res =>
                lazy val tokens = Token.StartObject :: fields.flatMap {
                  case (key, value) => Token.Key(key) :: tokenizer.tokenize(value).toList
                }.toList
                res match {
                  case Some((chunk, idx, rest, chunkAcc)) =>
                    Pull.pure(Some((chunk, idx, rest, Token.EndObject :: (tokens reverse_::: chunkAcc))))
                  case None =>
                    // this was the last value in the stream, emit the object and we are done
                    Pull.pure(Some((Chunk.empty, 0, Stream.empty, Token.EndObject :: tokens.reverse)))
                }
              }
            case Token.EndObject | Token.EndArray | Token.Key(_) =>
              // this is really malformed and should have been caught before
              // anyway, just raise the error
              Pull.raiseError[F](new JsonException("malformed json"))
          }
        case JsonMergePatch.Value(value) =>
          // the stream should now be pointing at the beginning of a json value
          // skip the currently pointed value and replace by the new one or delete it
          skipValue(chunk, idx, rest, 0, chunkAcc).flatMap {
            case Some((chunk, idx, rest, chunkAcc)) =>
              if (value == Json.Null)
                // if the value is null, then just drop it
                Pull.pure(Some((chunk, idx, rest, chunkAcc)))
              else
                // else replace
                Pull.pure(Some((chunk, idx, rest, tokenizer.tokenize(value).toList reverse_::: chunkAcc)))
            case None =>
              // EOS reached? this must have been the last value in the stream, just add the value
              Pull.pure(Some((Chunk.empty, 0, Stream.empty, tokenizer.tokenize(value).toList)))
          }
      }
    }

  private def go[F[_], Json](chunk: Chunk[Token],
                             idx: Int,
                             rest: Stream[F, Token],
                             patch: JsonMergePatch[Json],
                             chunkAcc: List[Token])(implicit F: ApplicativeError[F, Throwable],
                                                    Json: Jsony[Json],
                                                    tokenizer: Tokenizer[Json]): Pull[F, Token, Unit] =
    patchChunk(chunk, idx, rest, patch, chunkAcc).flatMap {
      case Some((chunk, idx, rest, chunkAcc)) =>
        go(chunk, idx, rest, patch, chunkAcc)
      case None =>
        Pull.done
    }

  def patch[F[_], Json](patch: JsonMergePatch[Json])(implicit F: ApplicativeError[F, Throwable],
                                                     Json: Jsony[Json],
                                                     tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] =
    s => go(Chunk.empty, 0, s, patch, Nil).stream

}
