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
package msgpack
package high

import cats.Monad
import fs2.data.msgpack.high.internal.Helpers._
import scala.util.control.NonFatal

/** Represents a process of extracting value of type `A` from an item stream.
  *
  * Built-in instances are accessbile via an import `fs2.data.msgpack.high.*`.
  */
trait MsgpackDeserializer[A] { outer =>
  private[high] def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]): DeserializationResult[F, A]

  def flatMap[B](f: A => MsgpackDeserializer[B]): MsgpackDeserializer[B] = new MsgpackDeserializer[B] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]): DeserializationResult[F, B] =
      outer.run(ctx).flatMap { case (x, ctx) => f(x).run(ctx) }
  }

  def map[B](f: A => B): MsgpackDeserializer[B] = new MsgpackDeserializer[B] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]): DeserializationResult[F, B] =
      outer.run(ctx).map { case (x, ctx) => (f(x), ctx) }
  }

  def either[B](implicit db: MsgpackDeserializer[B]) = new MsgpackDeserializer[Either[A, B]] {
    private[high] def run[F[_]: RaiseThrowable](
        ctx: DeserializationContext[F]): DeserializationResult[F, Either[A, B]] =
      outer
        .run(ctx)
        .map { case (item, ctx) => (Left(item), ctx) }
        .handleErrorWith {
          case NonFatal(_) =>
            db.run(ctx).map { case (item, ctx) => (Right(item), ctx) }
          case e => Pull.raiseError(e)
        }
  }
}

private[this] object MsgpackDeserializer {
  implicit val msgpackDeserializerMonad: Monad[MsgpackDeserializer] = new Monad[MsgpackDeserializer] {
    def pure[A](x: A): MsgpackDeserializer[A] = new MsgpackDeserializer[A] {
      def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]): DeserializationResult[F, A] =
        Pull.pure((x, ctx))
    }

    def flatMap[A, B](fa: MsgpackDeserializer[A])(f: A => MsgpackDeserializer[B]): MsgpackDeserializer[B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => MsgpackDeserializer[Either[A, B]]): MsgpackDeserializer[B] =
      new MsgpackDeserializer[B] {
        def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]): DeserializationResult[F, B] =
          f(a).run(ctx).flatMap {
            case (Left(x), ctx)  => tailRecM(x)(f).run(ctx)
            case (Right(x), ctx) => Pull.pure((x, ctx))
          }
      }
  }
}
