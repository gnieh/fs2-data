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

import fs2.data.msgpack.low.MsgpackItem
import fs2.data.msgpack.high.internal.Helpers._
import fs2.data.msgpack.high.internal.DeserializerBits._
import scodec.bits.ByteVector
import fs2.data.msgpack.high.DeserializationResult._

private[high] class DeserializerInstances {
  implicit val bigIntDeserializer: MsgpackDeserializer[BigInt] =
    getItem { (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
      item match {
        case MsgpackItem.UnsignedInt(bytes) =>
          if (bytes.length > 8) {
            Err("Number exceeds 64 bits")
          } else {
            val num = bytes.toLong(false)
            Ok(BigInt(num), tail)
          }

        case MsgpackItem.SignedInt(bytes) =>
          if (bytes.length > 8) {
            Err("Number exceeds 64 bits")
          } else {
            val num = bytes.toLong(true)
            Ok(BigInt(num), tail)
          }
        case _ => typeMismatch(item.getClass.getSimpleName, "BigInt")
      }
    }

  implicit def listDeserializer[A: MsgpackDeserializer]: MsgpackDeserializer[List[A]] =
    getItem { (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
      item match {
        case MsgpackItem.Array(size) if size < 0            => Err(s"Negative map length ${size}")
        case MsgpackItem.Array(size) if size > Int.MaxValue => Err(s"Array size exceeds Int.MaxValue")
        case MsgpackItem.Array(size)                        => runList[A](size, tail)
        case _                                              => typeMismatch(item.getClass.getSimpleName, "List")
      }
    }

  implicit val booleanDeserializer: MsgpackDeserializer[Boolean] = getItem {
    (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
      item match {
        case MsgpackItem.True  => Ok(true, tail)
        case MsgpackItem.False => Ok(false, tail)
        case _                 => typeMismatch(item.getClass.getSimpleName, "Boolean")
      }
  }

  implicit val byteDeserializer: MsgpackDeserializer[Byte] = getItem { (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
    item match {
      case MsgpackItem.SignedInt(bytes)   => runSignedByte(bytes, tail)
      case MsgpackItem.UnsignedInt(bytes) => runUnsignedByte(bytes, tail)
      case _                              => typeMismatch(item.getClass.getSimpleName, "Byte")
    }
  }

  implicit val byteVectorDeserializer: MsgpackDeserializer[ByteVector] = getItem {
    (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
      item match {
        case MsgpackItem.Bin(bytes) => Ok(bytes, tail)
        case _                      => typeMismatch(item.getClass.getSimpleName, "ByteVector")
      }
  }

  implicit val doubleDeserializer: MsgpackDeserializer[Double] = getItem {
    (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
      item match {
        case MsgpackItem.Float32(v) => Ok(v.toDouble, tail)
        case MsgpackItem.Float64(v) => Ok(v, tail)
        case _                      => typeMismatch(item.getClass.getSimpleName, "Double")
      }
  }

  implicit def eitherDeserializer[A, B](implicit
      da: MsgpackDeserializer[A],
      db: MsgpackDeserializer[B]): MsgpackDeserializer[Either[A, B]] =
    da.either[B]

  implicit val floatDeserializer: MsgpackDeserializer[Float] = getItem {
    (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
      item match {
        case MsgpackItem.Float32(v) => Ok(v, tail)
        case _                      => typeMismatch(item.getClass.getSimpleName, "Float")
      }
  }

  implicit val intDeserializer: MsgpackDeserializer[Int] = getItem { (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
    item match {
      case MsgpackItem.UnsignedInt(bytes) => runUnsignedInt(bytes, tail)
      case MsgpackItem.SignedInt(bytes)   => runSignedInt(bytes, tail)
      case _                              => typeMismatch(item.getClass.getSimpleName, "Int")
    }
  }

  implicit val longDeserializer: MsgpackDeserializer[Long] = getItem { (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
    item match {
      case MsgpackItem.UnsignedInt(bytes) => runUnsignedLong(bytes, tail)
      case MsgpackItem.SignedInt(bytes)   => runSignedLong(bytes, tail)
      case item                           => typeMismatch(item.getClass.getSimpleName, "Long")
    }
  }

  implicit def mapDeserializer[K, V](implicit
      dk: MsgpackDeserializer[K],
      dv: MsgpackDeserializer[V]): MsgpackDeserializer[Map[K, V]] = getItem {
    (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
      item match {
        case MsgpackItem.Map(size) if size < 0            => Err(s"Negative map length ${size}")
        case MsgpackItem.Map(size) if size > Int.MaxValue => Err(s"Map size exceeds Int.MaxValue")
        case MsgpackItem.Map(size)                        => runMap[K, V](size, tail)
        case _                                            => typeMismatch(item.getClass.getSimpleName, "Map")

      }
  }

  implicit val nullDeserializer: MsgpackDeserializer[Null] = getItem { (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
    item match {
      case MsgpackItem.Nil => Ok(null, tail)
      case _               => typeMismatch(item.getClass.getSimpleName, "Null")
    }
  }

  implicit def optionDeserializer[A](implicit da: MsgpackDeserializer[A]): MsgpackDeserializer[Option[A]] = (items) =>
    items match {
      case MsgpackItem.Nil +: tail => Ok(None, tail)
      case _                       => da.deserialize(items).mapValue(Some(_))

    }

  implicit val shortDeserializer: MsgpackDeserializer[Short] = getItem {
    (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
      item match {
        case MsgpackItem.UnsignedInt(bytes) => runUnsignedShort(bytes, tail)
        case MsgpackItem.SignedInt(bytes)   => runSignedShort(bytes, tail)
        case _                              => typeMismatch(item.getClass.getSimpleName, "Short")
      }
  }

  implicit val stringDeserializer: MsgpackDeserializer[String] = getItem {
    (item: MsgpackItem, tail: Vector[MsgpackItem]) =>
      item match {
        case MsgpackItem.Str(bytes) => runString(bytes, tail)
        case _                      => typeMismatch(item.getClass.getSimpleName, "String")
      }
  }
}
