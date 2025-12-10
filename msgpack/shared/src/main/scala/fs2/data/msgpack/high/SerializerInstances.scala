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

import scodec.bits.*

import low.MsgpackItem
import scala.collection.mutable.ArrayBuilder

private[high] trait SerializerInstances extends internal.PlatformSerializerInstances {
  def right1(x: MsgpackItem) = Right(Array(x))

  private def countBytesInt(x: Int): Int =
    if (x == 0) 1
    else if (x < 0) (32 - java.lang.Integer.numberOfLeadingZeros(x)) / 8
    else Math.ceil((32 - java.lang.Integer.numberOfLeadingZeros(x)) / 8.0).toInt

  private def countBytesLong(x: Long): Int =
    if (x == 0) 1
    else if (x < 0) (64 - java.lang.Long.numberOfLeadingZeros(x)) / 8
    else Math.ceil((64 - java.lang.Long.numberOfLeadingZeros(x)) / 8.0).toInt

  /* Numeric types
   *
   * We downcast larger types because we want squeeze the number into as little bytes as possible.
   * We could just drop leading zeros (which we do in the else branch), but that won't work
   * for negative numbers.
   */
  implicit val byteSerializer: MsgpackSerializer[Byte] = x => right1(MsgpackItem.SignedInt(ByteVector.fromByte(x)))

  implicit val shortSerializer: MsgpackSerializer[Short] = { x =>
    if (x.isValidByte)
      byteSerializer(x.toByte)
    else {
      val masked = x & 0xff
      val item =
        if (masked == x)
          MsgpackItem.UnsignedInt(ByteVector.fromShort(x, 1))
        else
          MsgpackItem.SignedInt(ByteVector.fromShort(x))
      right1(item)
    }
  }

  implicit val intSerializer: MsgpackSerializer[Int] = { x =>
    if (x.isValidShort)
      shortSerializer(x.toShort)
    else {
      val nbytes = countBytesInt(x)
      val bv = ByteVector.fromInt(x, nbytes)
      val item =
        if (nbytes == 2)
          MsgpackItem.UnsignedInt(bv)
        else
          MsgpackItem.SignedInt(bv)
      right1(item)
    }
  }

  implicit val longSerializer: MsgpackSerializer[Long] = { x =>
    if (x.isValidInt)
      intSerializer(x.toInt)
    else {
      val nbytes = countBytesLong(x)
      val bv = ByteVector.fromLong(x, nbytes)
      val item =
        if (nbytes == 4)
          MsgpackItem.UnsignedInt(bv)
        else
          MsgpackItem.SignedInt(bv)

      right1(item)
    }
  }

  implicit val bigIntSerializer: MsgpackSerializer[BigInt] = { x =>
    if (x.isValidLong)
      longSerializer(x.toLong)
    else {
      val bv = x.toByteArray.dropWhile(_ == 0)
      if (bv.length > 8)
        Left(s"BigInt exceeds 8 bytes (${bv.length} vs 8))")
      else
        right1(MsgpackItem.UnsignedInt(ByteVector(bv)))
    }
  }

  implicit val floatSerializer: MsgpackSerializer[Float] = x => right1(MsgpackItem.Float32(x))

  implicit val doubleSerializer: MsgpackSerializer[Double] = x => right1(MsgpackItem.Float64(x))

  /* Other scalar types */
  implicit val boolSerializer: MsgpackSerializer[Boolean] = x =>
    right1(
      if (x)
        MsgpackItem.True
      else
        MsgpackItem.False
    )

  implicit val nullSerializer: MsgpackSerializer[Null] = _ => right1(MsgpackItem.Nil)

  /* Compound types */
  implicit val stringSerializer: MsgpackSerializer[String] = { str =>
    ByteVector.encodeUtf8(str) match {
      case Left(e)      => Left(e.getMessage)
      case Right(bytes) => right1(MsgpackItem.Str(bytes))
    }
  }

  @inline implicit def mapSerializer[K, V](implicit
      sk: MsgpackSerializer[K],
      sv: MsgpackSerializer[V]): MsgpackSerializer[Map[K, V]] = { map =>
    val header = ArrayBuilder.make[MsgpackItem] += MsgpackItem.Array(map.size.toLong)

    val init: Either[String, ArrayBuilder[MsgpackItem]] = Right(header)

    map
      .foldLeft(init) { case (acc, (k, v)) =>
        for {
          builder <- acc
          key <- sk(k)
          value <- sv(v)
        } yield {
          builder ++= key
          builder ++= value
        }
      }
      .map(_.result())
  }

  @inline implicit def listSerializer[A](implicit sa: MsgpackSerializer[A]): MsgpackSerializer[List[A]] = { list =>
    val header = ArrayBuilder.make[MsgpackItem]

    val init: Either[String, (ArrayBuilder[MsgpackItem], Int)] = Right((header, 0))

    // List.size takes linear time, so we do that manually to avoid traversing
    // through the list 2 times.
    val folded = list.foldLeft(init) { (acc, x) =>
      for {
        tuple <- acc // scala3 seems to have a problem with "(a, b) <- c" syntax
        (builder, len) = tuple
        item <- sa(x)
      } yield {
        (builder ++= item, len + 1)
      }
    }

    folded.map { case (builder, length) =>
      val arr = builder.result()
      arr.prepended(MsgpackItem.Array(length.toLong))
    }
  }

  implicit val byteVectorSerializer: MsgpackSerializer[ByteVector] = bv => right1(MsgpackItem.Bin(bv))

  implicit def optionSerializer[A](implicit sa: MsgpackSerializer[A]): MsgpackSerializer[Option[A]] = _ match {
    case None        => right1(MsgpackItem.Nil)
    case Some(value) => sa(value)
  }

  implicit def eitherSerializer[L, R](implicit
      sl: MsgpackSerializer[L],
      sr: MsgpackSerializer[R]): MsgpackSerializer[Either[L, R]] = _ match {
    case Left(value)  => sl(value)
    case Right(value) => sr(value)
  }
}
