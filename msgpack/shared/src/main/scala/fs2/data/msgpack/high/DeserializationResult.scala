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

package fs2.data.msgpack.high
import fs2.data.msgpack.low.MsgpackItem

sealed trait DeserializationResult[+A] {
  @inline def mapValue[B](f: A => B): DeserializationResult[B] = this match {
    case DeserializationResult.Ok(value, reminder) => DeserializationResult.Ok(f(value), reminder)
    case x: DeserializationResult.Err              => x
    case x: DeserializationResult.NeedsMoreItems   => x
  }

  @inline def flatMap[B](f: (A, Vector[MsgpackItem]) => DeserializationResult[B]) = this match {
    case DeserializationResult.Ok(value, reminder) => f(value, reminder)
    case x: DeserializationResult.Err              => x
    case x: DeserializationResult.NeedsMoreItems   => x
  }
}

object DeserializationResult {
  final case class Ok[A](value: A, reminder: Vector[MsgpackItem]) extends DeserializationResult[A]
  final case class Err(msg: String) extends DeserializationResult[Nothing]
  final case class NeedsMoreItems(hint: Option[Long]) extends DeserializationResult[Nothing]

  @inline def pure[A](x: A, items: Vector[MsgpackItem]) = Ok(x, items)
}
