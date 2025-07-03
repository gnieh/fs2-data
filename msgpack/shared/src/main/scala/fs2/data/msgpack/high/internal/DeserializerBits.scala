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
package internal

import fs2.data.msgpack.high.internal.Helpers._
import scodec.bits._
import scala.collection.mutable
import scala.collection.immutable.HashMap
import fs2.data.msgpack.low.MsgpackItem
import fs2.data.msgpack.high.DeserializationResult._

/** Deserializer parts shared by fs2.data.msgpack.high and fs2.data.msgpack.high.ast.
  */
private[high] object DeserializerBits {
  def runUnsignedLong(bytes: ByteVector, rest: Vector[MsgpackItem]) =
    if (bytes.length > 8)
      Err("Number exceeds 64 bits")
    else if (bytes.length == 8 && firstBitPositive(bytes))
      typeMismatch("uint bigger than Long.MaxValue", "Long")
    else
      Ok(bytes.toLong(false), rest)

  def runSignedLong(bytes: ByteVector, rest: Vector[MsgpackItem]) =
    if (bytes.length > 8)
      Err("Number exceeds 64 bits")
    else
      Ok(bytes.toLong(true), rest)

  def runUnsignedInt(bytes: ByteVector, rest: Vector[MsgpackItem]) =
    if (bytes.length > 8)
      Err("Number exceeds 64 bits")
    else if (bytes.length > 4)
      typeMismatch(s"${bytes.length}-byte value", "Int")
    else if (bytes.length == 4 && firstBitPositive(bytes))
      typeMismatch("uint bigger than Int.MaxValue", "Int")
    else
      Ok(bytes.toInt(false), rest)

  def runSignedInt(bytes: ByteVector, rest: Vector[MsgpackItem]) =
    if (bytes.length > 8)
      Err("Number exceeds 64 bits")
    else if (bytes.length > 4)
      typeMismatch(s"${bytes.length}-byte value", "Int")
    else
      Ok(bytes.toInt(true), rest)

  def runUnsignedShort(bytes: ByteVector, rest: Vector[MsgpackItem]) =
    if (bytes.length > 8)
      Err("Number exceeds 64 bits")
    else if (bytes.length > 2)
      typeMismatch(s"${bytes.length}-byte value", "Short")
    else if (bytes.length == 2 && firstBitPositive(bytes))
      typeMismatch("uint bigger than Short.MaxValue", "Short")
    else
      Ok(bytes.toShort(false), rest)

  def runSignedShort(bytes: ByteVector, rest: Vector[MsgpackItem]) =
    if (bytes.length > 8)
      Err("Number exceeds 64 bits")
    else if (bytes.length > 2)
      typeMismatch(s"${bytes.length}-byte value", "Short")
    else
      Ok(bytes.toShort(true), rest)

  def runUnsignedByte(bytes: ByteVector, rest: Vector[MsgpackItem]) =
    if (bytes.length > 8)
      Err("Number exceeds 64 bits")
    else if (bytes.length > 1)
      typeMismatch(s"${bytes.length}-byte value", "Byte")
    else if (bytes.length == 1 && firstBitPositive(bytes))
      typeMismatch("uint bigger than Byte.MaxValue", "Byte")
    else
      Ok(bytes.toByte(false), rest)

  def runSignedByte(bytes: ByteVector, rest: Vector[MsgpackItem]) =
    if (bytes.length > 8)
      Err("Number exceeds 64 bits")
    else if (bytes.length > 1)
      typeMismatch(s"${bytes.length}-byte value", "Byte")
    else
      Ok(bytes.toByte(true), rest)

  def runList[A](size: Long, rest: Vector[MsgpackItem])(implicit da: MsgpackDeserializer[A]) = {
    def go(n: Long, acc: mutable.Builder[A, List[A]], rest: Vector[MsgpackItem]): DeserializationResult[List[A]] = {
      if (n == 0)
        Ok(acc.result(), rest)
      else if (rest.length == 0)
        NeedsMoreItems(None)
      else
        da.deserialize(rest).flatMap { (value, reminder) =>
          go(n - 1, acc += value, reminder)
        }
    }

    if (size > rest.length)
      NeedsMoreItems(Some(size))
    else
      go(size, List.newBuilder[A], rest)
  }

  def runMap[K, V](size: Long, rest: Vector[MsgpackItem])(implicit
      dk: MsgpackDeserializer[K],
      dv: MsgpackDeserializer[V]) = {
    def go(n: Long,
           acc: mutable.Builder[(K, V), HashMap[K, V]],
           rest: Vector[MsgpackItem]): DeserializationResult[Map[K, V]] = {
      if (n == 0)
        Ok(acc.result(), rest)
      else if (rest.length == 0)
        NeedsMoreItems(None)
      else
        dk.deserialize(rest).flatMap { (key, rest1) =>
          dv.deserialize(rest1).flatMap { (value, rest2) =>
            go(n - 1, acc += ((key, value)), rest2)
          }
        }
    }

    go(size, HashMap.newBuilder[K, V], rest) match {
      case NeedsMoreItems(Some(x)) => NeedsMoreItems(Some(size * 2 + x - 1))
      case NeedsMoreItems(None)    => NeedsMoreItems(Some(size * 2 + 1))
      case x                       => x
    }
  }

  def runString(bytes: ByteVector, rest: Vector[MsgpackItem]) =
    bytes.decodeUtf8 match {
      case Left(e)    => Err(e.getMessage)
      case Right(str) => Ok(str, rest)
    }
}
