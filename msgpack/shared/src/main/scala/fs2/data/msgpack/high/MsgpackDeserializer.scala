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
import fs2.data.msgpack.high.DeserializationResult._
import fs2.data.msgpack.low.MsgpackItem
import scala.annotation.tailrec

/** Represents a process of extracting value of type `A` from an item stream.
  *
  * Built-in instances are accessible via an import `fs2.data.msgpack.high.*`.
  */
trait MsgpackDeserializer[A] { self =>
  def deserialize(items: Chunk[MsgpackItem]): DeserializationResult[A]

  def map[B](f: A => B): MsgpackDeserializer[B] = (items: Chunk[MsgpackItem]) =>
    self.deserialize(items) match {
      case Ok(value, reminder) => Ok(f(value), reminder)
      case x: Err              => x
      case x: NeedsMoreItems   => x
    }

  def flatMap[B](f: A => MsgpackDeserializer[B]): MsgpackDeserializer[B] =
    new FlatMapDeserializer(self, f)

  def either[B](implicit db: MsgpackDeserializer[B]): MsgpackDeserializer[Either[A, B]] =
    (items: Chunk[MsgpackItem]) =>
      self.deserialize(items) match {
        case Ok(value, reminder) => Ok(Left(value), reminder)
        case _                   => db.deserialize(items).mapValue(Right(_))
      }

}

/** Provides a tail-recursive `deserialize` method for chained `flatMap` calls. */
private case class FlatMapDeserializer[A, B](da: MsgpackDeserializer[A], fa: A => MsgpackDeserializer[B])
    extends MsgpackDeserializer[B] {
  @tailrec
  private def go[C, D](dc: MsgpackDeserializer[C],
                       fc: C => MsgpackDeserializer[D],
                       items: Chunk[MsgpackItem]): DeserializationResult[D] = {
    dc.deserialize(items) match {
      case Ok(value, reminder) =>
        fc(value) match {
          case FlatMapDeserializer(dd, fd) => go(dd, fd, reminder)
          case d                           => d.deserialize(reminder)
        }
      case x: Err            => x
      case x: NeedsMoreItems => x
    }
  }

  @inline def deserialize(items: Chunk[MsgpackItem]): DeserializationResult[B] =
    go[A, B](da, fa, items)
}

object MsgpackDeserializer {
  def apply[A](implicit ev: MsgpackDeserializer[A]) = ev

  implicit val msgpackDeserializerMonad: Monad[MsgpackDeserializer] = new Monad[MsgpackDeserializer] {
    def pure[A](x: A): MsgpackDeserializer[A] = (items: Chunk[MsgpackItem]) => Ok(x, items)

    def flatMap[A, B](fa: MsgpackDeserializer[A])(f: A => MsgpackDeserializer[B]): MsgpackDeserializer[B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => MsgpackDeserializer[Either[A, B]]): MsgpackDeserializer[B] = {
      f(a).flatMap {
        case Left(value)  => tailRecM(value)(f)
        case Right(value) => pure(value)
      }
    }

  }
}
