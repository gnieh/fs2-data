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

import text._
import json.ast._
import json.internals._

import cats._

/** Handles stream parsing and traversing of json documents.
  */
package object json {

  /** Transforms a stream of characters into a stream of Json tokens.
    * Emitted tokens are guaranteed to be valid up to that point.
    * If the streams ends without failure, the sequence of tokens is sensured
    * to represent a (potentially empty) sequence of valid Json documents.
    */
  def tokens[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): Pipe[F, T, Token] =
    TokenParser.pipe[F, T]

  /** Filters the tokens according to the given selector sequence.
    * if `wrap` is set to `true` then values selected by array selector are wrapped into
    * an array, and values selected by object selector are wrapped into an object with original
    * key maintained.
    */
  def filter[F[_]](selector: Selector, wrap: Boolean = false)(implicit F: RaiseThrowable[F]): Pipe[F, Token, Token] =
    TokenSelector.pipe[F](selector, wrap)

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * The rest of the stream is left unchanged.
    *
    * This operator locally creates Json AST values using the [[Builder]], and
    * returns tokens as emitted by the [[Tokenizer]] on the resulting value.
    */
  @deprecated(message = "Use `fs2.data.json.ast.transform` instead", since = "1.3.0")
  def transform[F[_], Json](selector: Selector, f: Json => Json)(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] =
    ast.transform(selector, f)

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * If the function returns `None`, then the entire value is dropped (and the object key it
    * is located at, if any).
    * The rest of the stream is left unchanged.
    *
    * This operator locally creates Json AST values using the [[Builder]], and
    * returns tokens as emitted by the [[Tokenizer]] on the resulting value.
    */
  @deprecated(message = "Use `fs2.data.json.ast.transformOpt` instead", since = "1.3.0")
  def transformOpt[F[_], Json](selector: Selector, f: Json => Option[Json])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] =
    ast.transformOpt(selector, f)

  /** Transforms a stream of token into another one. The transformation function `f` is
    * called on every selected value from upstream, and the resulting value replaces it.
    * The rest of the stream is left unchanged. The operation can fail, in case the returned
    * `F` is failed at one step.
    *
    * This operator locally creates Json AST values using the [[Builder]], and
    * returns tokens as emitted by the [[Tokenizer]] on the resulting value.
    */
  @deprecated(message = "Use `fs2.data.json.ast.transformF` instead", since = "1.3.0")
  def transformF[F[_], Json](selector: Selector, f: Json => F[Json])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] =
    ast.transformF(selector, f)

  /** Transforms a stream of Json tokens into a stream of json values.
    */
  @deprecated(message = "Use `fs2.data.json.ast.values` instead", since = "1.3.0")
  def values[F[_], Json](implicit F: RaiseThrowable[F], builder: Builder[Json]): Pipe[F, Token, Json] =
    ast.values[F, Json]

  /** Transforms a stream of Json values into a stream of Json tokens.
    *
    * This operation is the opposite of `values`.
    */
  @deprecated(message = "Use `fs2.data.json.ast.tokenize` instead", since = "1.3.0")
  def tokenize[F[_], Json](implicit tokenizer: Tokenizer[Json]): Pipe[F, Json, Token] =
    ast.tokenize[F, Json]

  /** A collection of pipes to wrap streams inside objects. */
  object wrap {

    /** Wraps the stream elements as an array inside an object at the given `at` key.
      * The object also contains the keys from the `in` map if any.
      * If `mapFirst` is true, then the map elements are emitted first, then the stream, otherwise the stream is emitted first.
      *
      * The resulting token stream is a valid single JSON object stream, iff the original
      * stream is a valid stream of JSON values.
      */
    def asArrayInObject[F[_], Json](at: String,
                                    in: Map[String, Json] = Map.empty[String, Json],
                                    mapFirst: Boolean = true)(implicit
        tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] =
      ObjectWrapper.pipe[F, Json](at, in, mapFirst, false)

    /** Wraps the stream element as a single value inside an object at the given `at` key.
      * The object also contains the keys from the `in` map if any.
      * If `mapFirst` is true, then the map elements are emitted first, then the stream, otherwise the stream is emitted first.
      *
      * The resulting token stream is a valid single JSON object stream, iff the original
      * stream is a valid stream of a **single** JSON value.
      */
    def asValueInObject[F[_], Json](at: String,
                                    in: Map[String, Json] = Map.empty[String, Json],
                                    mapFirst: Boolean = true)(implicit
        tokenizer: Tokenizer[Json]): Pipe[F, Token, Token] =
      ObjectWrapper.pipe[F, Json](at, in, mapFirst, true)

    /** Wraps the stream elements as an array at top-level.
      *
      * The resulting token stream is a valid single JSON array stream, iff the original
      * stream is a valid stream of JSON values.
      */
    def asTopLevelArray[F[_]]: Pipe[F, Token, Token] =
      s => Stream.emit(Token.StartArray) ++ s ++ Stream.emit(Token.EndArray)

  }

  /** Json Token stream pipes to render Json values. */
  object render {

    /** Renders a compact representation of the Json token stream.
      *
      * Chunks can be concatenated to render all values in the stream,
      * separated by new lines.
      *
      * You can use this to write the Json stream to a file.
      */
    def compact[F[_]]: Pipe[F, Token, String] =
      Renderer.pipe[F](false, "")

    /** Renders a pretty-printed representation of the token stream with the given
      * indentation size.
      *
      * Chunks can be concatenated to render all values in the stream,
      * separated by new lines.
      *
      * You can use this to write the Json stream to a file.
      */
    def pretty[F[_]](indent: String = "  "): Pipe[F, Token, String] =
      Renderer.pipe[F](true, indent)

  }

  /** Json Token stream collectors. */
  object collector {

    /** A collector of the Json values in the streams
      * rendered in compact form.
      *
      * Top-level values are separated by new lines.
      */
    object compact extends Collector[Token] {
      type Out = String
      def newBuilder: Collector.Builder[Token, Out] =
        new Renderer(false, false, "")
    }

    /** A collector of the Json values in the streams
      * rendered in pretty form.
      *
      * Top-level values are separated by new lines.
      */
    def pretty(indent: String = "  "): Collector.Aux[Token, String] =
      new Collector[Token] {
        type Out = String
        def newBuilder: Collector.Builder[Token, String] =
          new Renderer(true, false, indent)
      }
  }

  implicit class JsonSelectorStringOps(val s: String) extends AnyVal {
    def parseSelector[F[_]](implicit F: MonadError[F, Throwable]): F[Selector] =
      new SelectorParser[F](s).parse()
  }

}
