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
package data.json

import internals.{TokenSelector, ValueParser}

import cats.syntax.all._

package object ast {

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * The rest of the stream is left unchanged.
    *
    * This operator locally creates Json AST values using the [[Builder]], and
    * returns tokens as emitted by the [[Tokenizer]] on the resulting value.
    */
  def transform[F[_], Json](selector: Selector, f: Json => Json)(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] =
    TokenSelector.transformPipe[F, Json, Json, Json](selector, _.asRight, f, _.some, true)

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * If the function returns `None`, then the entire value is dropped (and the object key it
    * is located at, if any).
    * The rest of the stream is left unchanged.
    *
    * This operator locally creates Json AST values using the [[Builder]], and
    * returns tokens as emitted by the [[Tokenizer]] on the resulting value.
    */
  def transformOpt[F[_], Json](selector: Selector, f: Json => Option[Json])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] =
    TokenSelector.transformPipe[F, Json, Json, Option[Json]](selector, _.asRight, f, identity, false)

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * The rest of the stream is left unchanged. The operation can fail, in case the returned
    * `F` is failed at one step.
    *
    * This operator locally creates Json AST values using the [[Builder]], and
    * returns tokens as emitted by the [[Tokenizer]] on the resulting value.
    */
  def transformF[F[_], Json](selector: Selector, f: Json => F[Json])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] =
    TokenSelector.transformPipeF[F, Json, Json, Json](selector, _.asRight, f, _.some, true)

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * The rest of the stream is left unchanged. The operation can fail, in case the returned
    * `F` is failed at one step.
    *
    * This operator locally creates Json AST values using the [[Builder]], and
    * returns tokens as emitted by the [[Tokenizer]] on the resulting value.
    */
  def transformOptF[F[_], Json](selector: Selector, f: Json => F[Option[Json]])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] =
    TokenSelector.transformPipeF[F, Json, Json, Option[Json]](selector, _.asRight, f, identity, false)

  /** Transforms a stream of Json tokens into a stream of json values.
    */
  def values[F[_], Json](implicit F: RaiseThrowable[F], builder: Builder[Json]): Pipe[F, Token, Json] =
    ValueParser.pipe[F, Json]

  /** Transforms a stream of Json values into a stream of Json tokens.
    *
    * This operation is the opposite of `values`.
    */
  def tokenize[F[_], Json](implicit tokenizer: Tokenizer[Json]): Pipe[F, Json, Token] =
    _.flatMap(value => Stream.emits(tokenizer.tokenize(value).toList))

}
