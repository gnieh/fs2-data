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
package data
package json
package jq
package internal

import cats.data.NonEmptyChain

import tagged._

private[jq] class ESPCompiledJq[F[_]: RaiseThrowable](val esp: JqESP[F]) extends CompiledJq[F] {

  def apply(in: Stream[F, Token]): Stream[F, Token] =
    in.through(JsonTagger.pipe)
      .through(esp.pipe[TaggedJson, TaggedJson])
      .map(untag(_))
      .unNone

  def andThen(that: CompiledJq[F]): CompiledJq[F] =
    that match {
      case that: PipedCompiledJq[F] =>
        new PipedCompiledJq[F](this +: that.jqs)
      case _ =>
        // no idea how to fuse them, be naive
        new PipedCompiledJq[F](NonEmptyChain(this, that))
    }

}
