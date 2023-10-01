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
package data
package json
package jq
package internal

import cats.data.NonEmptyChain
import cats.syntax.all._

import tagged._

private[jq] class FusedESPCompiledJq[F[_]: RaiseThrowable](val esps: NonEmptyChain[JqESP[F]]) extends CompiledJq[F] {

  override def apply(in: Stream[F, Token]): Stream[F, Token] =
    esps
      .foldLeft(in.through(JsonTagger.pipe))((base, esp) => base.through(esp.pipe))
      .map(untag(_))
      .unNone

  override def andThen(that: CompiledJq[F]): CompiledJq[F] =
    that match {
      // case that: ESPCompiledJq[F] =>
      //  new FusedESPCompiledJq[F](this.esps :+ that.esp)
      // case that: FusedESPCompiledJq[F] =>
      //  new FusedESPCompiledJq[F](this.esps ++ that.esps)
      case that: PipedCompiledJq[F] =>
        new PipedCompiledJq[F](this +: that.jqs)
      case _ =>
        new PipedCompiledJq[F](NonEmptyChain(this, that))
    }

}
