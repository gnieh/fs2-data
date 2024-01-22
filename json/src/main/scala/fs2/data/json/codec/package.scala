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
package data.json

import internals._

import cats.syntax.all._

package object codec {

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * The rest of the stream is left unchanged.
    *
    * This operator locally deserializes the Json values using the [[Deserializer]], and
    * returns tokens as emitted by the [[Serializer]] on the resulting value.
    */
  def transform[F[_], A, B, Json](selector: Selector, f: A => B)(implicit
      F: RaiseThrowable[F],
      deserializer: Deserializer.Aux[A, Json],
      serializer: Serializer.Aux[B, Json]): Pipe[F, Token, Token] = {
    import deserializer.builder
    import serializer.tokenizer
    TokenSelector
      .transformPipe[F, Json, A, B](selector, deserializer.deserialize, f, serializer.serialize(_).some, true)
  }

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * If the function returns `None`, then the entire value is dropped (and the object key it
    * is located at, if any).
    * The rest of the stream is left unchanged.
    *
    * This operator locally deserializes Json values using the [[Deserializer]], and
    * returns tokens as emitted by the [[Serializer]] on the resulting value.
    */
  def transformOpt[F[_], A, B, Json](selector: Selector, f: A => Option[B])(implicit
      F: RaiseThrowable[F],
      deserializer: Deserializer.Aux[A, Json],
      serializer: Serializer.Aux[B, Json]): Pipe[F, Token, Token] = {
    import deserializer.builder
    import serializer.tokenizer
    TokenSelector
      .transformPipe[F, Json, A, Option[B]](selector, deserializer.deserialize, f, _.map(serializer.serialize), false)
  }

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * The rest of the stream is left unchanged. The operation can fail, in case the returned
    * `F` is failed at one step.
    *
    * This operator locally deserializes Json values using the [[Deserializer]], and
    * returns tokens as emitted by the [[Serializer]] on the resulting value.
    */
  def transformF[F[_], A, B, Json](selector: Selector, f: A => F[B])(implicit
      F: RaiseThrowable[F],
      deserializer: Deserializer.Aux[A, Json],
      serializer: Serializer.Aux[B, Json]): Pipe[F, Token, Token] = {
    import deserializer.builder
    import serializer.tokenizer
    TokenSelector
      .transformPipeF[F, Json, A, B](selector, deserializer.deserialize, f, serializer.serialize(_).some, true)
  }

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * The rest of the stream is left unchanged. The operation can fail, in case the returned
    * `F` is failed at one step.
    *
    * This operator locally deserializes Json values using the [[Deserializer]], and
    * returns tokens as emitted by the [[Serializer]] on the resulting value.
    */
  def transformOptF[F[_], A, B, Json](selector: Selector, f: A => F[Option[B]])(implicit
      F: RaiseThrowable[F],
      deserializer: Deserializer.Aux[A, Json],
      serializer: Serializer.Aux[B, Json]): Pipe[F, Token, Token] = {
    import deserializer.builder
    import serializer.tokenizer
    TokenSelector
      .transformPipeF[F, Json, A, Option[B]](selector, deserializer.deserialize, f, _.map(serializer.serialize), false)
  }

  /** Transforms a stream of Json tokens into a stream of deserialized values.
    */
  def deserialize[F[_], A](implicit F: RaiseThrowable[F], deserializer: Deserializer[A]): Pipe[F, Token, A] = {
    import deserializer.builder
    _.through(ast.values[F, deserializer.Json]).map(deserializer.deserialize).rethrow
  }

  /** Transforms a stream of values into a stream of Json tokens.
    *
    * This operation is the opposite of `deserialize`.
    */
  def serialize[F[_], A](implicit serializer: Serializer[A]): Pipe[F, A, Token] = {
    import serializer.tokenizer
    _.map(serializer.serialize(_)).through(ast.tokenize)
  }

}
