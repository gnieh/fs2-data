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

private[high] class DeserializerInstances {
  implicit object bigIntDeserializer extends MsgpackDeserializer[BigInt] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.SignedInt(bytes)   => runSignedLong(bytes, ctx).map { case (x, ctx) => (BigInt(x), ctx) }
        case MsgpackItem.UnsignedInt(bytes) => runBigInt(bytes, ctx)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "BigInt"))
      }
    }
  }

  implicit object booleanDeserializer extends MsgpackDeserializer[Boolean] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.True  => ctx.proceed(true)
        case MsgpackItem.False => ctx.proceed(false)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Boolean"))
      }
    }
  }

  implicit object byteDeserializer extends MsgpackDeserializer[Byte] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.SignedInt(bytes)   => runSignedByte(bytes, ctx)
        case MsgpackItem.UnsignedInt(bytes) => runUnsignedByte(bytes, ctx)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Byte"))
      }
    }
  }

  implicit object byteVectorDeserializer extends MsgpackDeserializer[ByteVector] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Bin(bytes) => ctx.proceed(bytes)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "ByteVector"))
      }
    }
  }

  implicit object doubleDeserializer extends MsgpackDeserializer[Double] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Float32(v) => ctx.proceed(v.toDouble)
        case MsgpackItem.Float64(v) => ctx.proceed(v)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Double"))
      }
    }
  }

  implicit def eitherDeserializer[A, B](implicit
      da: MsgpackDeserializer[A],
      db: MsgpackDeserializer[B]): MsgpackDeserializer[Either[A, B]] =
    da.either[B]

  implicit def extensionDeserializer[A](f: (Byte, ByteVector) => Option[A]): MsgpackDeserializer[A] =
    new MsgpackDeserializer[A] {
      def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) =
        get1(ctx) { (item, ctx) =>
          item match {
            case MsgpackItem.Extension(tpe, bytes) =>
              f(tpe, bytes) match {
                case Some(x) => ctx.proceed(x)
                case None    => Pull.raiseError(new MsgpackDeserializationTypeMismatchException)
              }
            case x =>
              Pull.raiseError(
                new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}",
                                                                "Extension"))
          }
        }
    }

  implicit object floatDeserializer extends MsgpackDeserializer[Float] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Float32(v) => ctx.proceed(v)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Float"))
      }
    }
  }

  implicit object intDeserializer extends MsgpackDeserializer[Int] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.UnsignedInt(bytes) => runUnsignedInt(bytes, ctx)
        case MsgpackItem.SignedInt(bytes)   => runSignedInt(bytes, ctx)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Int"))
      }
    }
  }

  implicit def listDeserializer[A](implicit da: MsgpackDeserializer[A]): MsgpackDeserializer[List[A]] =
    new MsgpackDeserializer[List[A]] {
      def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
        item match {
          case MsgpackItem.Array(size) if size < 0 =>
            Pull.raiseError(new MsgpackMalformedItemException(s"Negative array length ${size}"))
          case MsgpackItem.Array(size) if size > Int.MaxValue =>
            Pull.raiseError(new MsgpackMalformedItemException("Array size exceeds Int.MaxValue"))
          case MsgpackItem.Array(size) => runList[F, A](size.toInt, ctx)
          case x =>
            Pull.raiseError(
              new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "List"))
        }
      }
    }

  implicit object longDeserializer extends MsgpackDeserializer[Long] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.UnsignedInt(bytes) => runUnsignedLong(bytes, ctx)
        case MsgpackItem.SignedInt(bytes)   => runSignedLong(bytes, ctx)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Long"))
      }
    }
  }

  implicit def mapDeserializer[K, V](implicit
      pk: MsgpackDeserializer[K],
      pv: MsgpackDeserializer[V]): MsgpackDeserializer[Map[K, V]] =
    new MsgpackDeserializer[Map[K, V]] {
      def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
        item match {
          case MsgpackItem.Map(size) if size < 0 =>
            Pull.raiseError(new MsgpackMalformedItemException(s"Negative map length ${size}"))
          case MsgpackItem.Map(size) if size > Int.MaxValue =>
            Pull.raiseError(new MsgpackMalformedItemException("Map size exceeds Int.MaxValue"))
          case MsgpackItem.Map(size) => runMap[F, K, V](size.toInt, ctx)
          case x =>
            Pull.raiseError(
              new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Map"))
        }
      }
    }

  implicit object nilDeserializer extends MsgpackDeserializer[Null] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Nil => ctx.proceed(null)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Null"))
      }
    }
  }

  implicit def optionDeserializer[A](implicit da: MsgpackDeserializer[A]): MsgpackDeserializer[Option[A]] =
    new MsgpackDeserializer[Option[A]] {
      def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
        if (item == MsgpackItem.Nil)
          ctx.proceed(None)
        else
          da.run(ctx).map { case (item, ctx) => (Some(item), ctx) }
      }
    }

  implicit object shortDeserializer extends MsgpackDeserializer[Short] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.UnsignedInt(bytes) => runUnsignedShort(bytes, ctx)
        case MsgpackItem.SignedInt(bytes)   => runSignedShort(bytes, ctx)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Short"))
      }
    }
  }

  implicit object stringDeserializer extends MsgpackDeserializer[String] {
    def run[F[_]: RaiseThrowable](ctx: DeserializationContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Str(bytes) => runString(bytes, ctx)
        case x =>
          Pull.raiseError(
            new MsgpackDeserializationTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "String"))
      }
    }
  }
}
