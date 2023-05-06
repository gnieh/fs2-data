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

import tagged._

private[jq] class ESPCompiledJq[F[_]: RaiseThrowable](val esp: JqESP[F]) extends CompiledJq[F] {

  def apply(in: Stream[F, Token]): Stream[F, Token] =
    in.through(JsonTagger.pipe)
      .through(esp.pipe)
      .mapAccumulate(0) {
        case (0, TaggedJson.StartObjectValue(_)) =>
          (0, None)
        case (depth, TaggedJson.Raw(Token.StartObject)) =>
          (depth + 1, Some(Token.StartObject))
        case (depth, TaggedJson.Raw(Token.EndObject)) =>
          (depth - 1, Some(Token.EndObject))
        case (depth, TaggedJson.Raw(t)) =>
          (depth, Some(t))
        case (depth, TaggedJson.StartArrayElement(_)) =>
          (depth, None)
        case (depth, TaggedJson.EndArrayElement) =>
          (depth, None)
        case (depth, TaggedJson.StartObjectValue(name)) =>
          (depth, Some(Token.Key(name)))
        case (depth, TaggedJson.EndObjectValue) =>
          (depth, None)
        case (depth, TaggedJson.StartJson) =>
          (depth, None)
        case (depth, TaggedJson.EndJson) =>
          (depth, None)
      }
      .map(_._2)
      .unNone

  def andThen(that: CompiledJq[F]): CompiledJq[F] =
    that match {
      case that: ESPCompiledJq[F] =>
        // fuse them to avoid tagging/untagging between each stage
        new FusedESPCompiledJq[F](NonEmptyChain(this.esp, that.esp))
      case that: FusedESPCompiledJq[F] =>
        new FusedESPCompiledJq[F](this.esp +: that.esps)
      case that: PipedCompiledJq[F] =>
        new PipedCompiledJq[F](this +: that.jqs)
      case _ =>
        // no idea how to fuse them, be naive
        new PipedCompiledJq[F](NonEmptyChain(this, that))
    }

}
