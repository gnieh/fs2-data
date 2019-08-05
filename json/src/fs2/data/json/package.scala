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
import cats.effect._
import cats.data.NonEmptyList
import cats.implicits._

import scala.annotation.switch
import scala.collection.compat._

import scala.language.{higherKinds, implicitConversions}

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

  /** Filters the tokens according to the given selector sequence.
    * if `wrap` is set to `true` then values selected by array selector are wrapped into
    * an array, and values selected by object selector are wrapped into an object with original
    * key maintained.
    */
  def filter[F[_]](selector: Selector, wrap: Boolean = false)(
      implicit F: ApplicativeError[F, Throwable]): Pipe[F, Token, Token] =
    TokenSelector.pipe[F](selector, wrap)

  /** Transforms a stream of Json tokens into a stream of json values.
    */
  def values[F[_], Json](implicit F: ApplicativeError[F, Throwable], builder: Builder[Json]): Pipe[F, Token, Json] =
    ValueParser.pipe[F, Json]

  implicit class StringOps(val s: String) extends AnyVal {
    def parseSelector[F[_]](implicit F: MonadError[F, Throwable]): F[Selector] =
      new SelectorParser[F](s).parse()
  }

}
