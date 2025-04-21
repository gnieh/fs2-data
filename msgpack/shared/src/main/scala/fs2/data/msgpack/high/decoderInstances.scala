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
import fs2.data.msgpack.high.internal.DecoderBits._
import scodec.bits.ByteVector

private[high] class DynamicDecoderInstances {
  implicit object msgpackValueDecoder extends MsgpackDecoder[MsgpackValue] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.SignedInt(bytes) =>
          runSignedLong(bytes, ctx).map { case (item, ctx) => (MsgpackValue.Integer(item), ctx) }

        case MsgpackItem.UnsignedInt(bytes) =>
          runUnsignedLong(bytes, ctx).map { case (item, ctx) => (MsgpackValue.Integer(item), ctx) }

        case MsgpackItem.Float32(x) => ctx.proceed(MsgpackValue.Float(x))

        case MsgpackItem.Float64(x) => ctx.proceed(MsgpackValue.Double(x))

        case MsgpackItem.Array(size) if size < 0 =>
          Pull.raiseError(new MsgpackMalformedItemException(s"Negative array length ${size}"))

        case MsgpackItem.Array(size) if size > Int.MaxValue =>
          Pull.raiseError(new MsgpackMalformedItemException("Array size exceeds Int.MaxValue"))

        case MsgpackItem.Array(size) =>
          runList[F, MsgpackValue](size.toInt, ctx).map { case (item, ctx) =>
            (MsgpackValue.Array(item), ctx)
          }

        case MsgpackItem.Map(size) if size < 0 =>
          Pull.raiseError(new MsgpackMalformedItemException(s"Negative map length ${size}"))

        case MsgpackItem.Map(size) if size > Int.MaxValue =>
          Pull.raiseError(new MsgpackMalformedItemException("Map size exceeds Int.MaxValue"))

        case MsgpackItem.Map(size) =>
          runMap[F, MsgpackValue, MsgpackValue](size.toInt, ctx).map { case (item, ctx) =>
            (MsgpackValue.Map(item), ctx)
          }

        case MsgpackItem.Timestamp32(seconds) =>
          ctx.proceed(MsgpackValue.Timestamp(0, seconds.toLong))

        case item: MsgpackItem.Timestamp64 =>
          ctx.proceed(MsgpackValue.Timestamp(item.nanoseconds, item.seconds))

        case MsgpackItem.Timestamp96(nanoseconds, seconds) =>
          ctx.proceed(MsgpackValue.Timestamp(nanoseconds, seconds))

        case MsgpackItem.Bin(bytes) => ctx.proceed(MsgpackValue.Bin(bytes))

        case MsgpackItem.Str(bytes) =>
          runString(bytes, ctx).map { case (item, ctx) => (MsgpackValue.String(item), ctx) }

        case MsgpackItem.Extension(tpe, bytes) => ctx.proceed(MsgpackValue.Extension(tpe, bytes))

        case MsgpackItem.Nil   => ctx.proceed(MsgpackValue.Nil)
        case MsgpackItem.True  => ctx.proceed(MsgpackValue.Boolean(true))
        case MsgpackItem.False => ctx.proceed(MsgpackValue.Boolean(false))
      }
    }
  }
}

private[high] class StaticDecoderInstances {
  implicit object bigIntDecoder extends MsgpackDecoder[BigInt] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.SignedInt(bytes)   => runSignedLong(bytes, ctx).map { case (x, ctx) => (BigInt(x), ctx) }
        case MsgpackItem.UnsignedInt(bytes) => runBigInt(bytes, ctx)
        case x =>
          Pull.raiseError(
            new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "BigInt"))
      }
    }
  }

  implicit object booleanDecoder extends MsgpackDecoder[Boolean] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.True  => ctx.proceed(true)
        case MsgpackItem.False => ctx.proceed(false)
        case x =>
          Pull.raiseError(
            new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Boolean"))
      }
    }
  }

  implicit object byteDecoder extends MsgpackDecoder[Byte] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.SignedInt(bytes)   => runSignedByte(bytes, ctx)
        case MsgpackItem.UnsignedInt(bytes) => runUnsignedByte(bytes, ctx)
        case x =>
          Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Byte"))
      }
    }
  }

  implicit object byteVectorDecoder extends MsgpackDecoder[ByteVector] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Bin(bytes) => ctx.proceed(bytes)
        case x =>
          Pull.raiseError(
            new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "ByteVector"))
      }
    }
  }

  implicit object doubleDecoder extends MsgpackDecoder[Double] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Float32(v) => ctx.proceed(v.toDouble)
        case MsgpackItem.Float64(v) => ctx.proceed(v)
        case x =>
          Pull.raiseError(
            new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Double"))
      }
    }
  }

  implicit def eitherDecoder[A, B](implicit
      da: MsgpackDecoder[A],
      db: MsgpackDecoder[B]): MsgpackDecoder[Either[A, B]] =
    da.either[B]

  implicit def extensionDecoder[A](f: (Byte, ByteVector) => Option[A]): MsgpackDecoder[A] = new MsgpackDecoder[A] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) =
      get1(ctx) { (item, ctx) =>
        item match {
          case MsgpackItem.Extension(tpe, bytes) =>
            f(tpe, bytes) match {
              case Some(x) => ctx.proceed(x)
              case None    => Pull.raiseError(new MsgpackDecodingTypeMismatchException)
            }
          case x =>
            Pull.raiseError(
              new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Extension"))
        }
      }
  }

  implicit object floatDecoder extends MsgpackDecoder[Float] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Float32(v) => ctx.proceed(v)
        case x =>
          Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Float"))
      }
    }
  }
  
  implicit object intDecoder extends MsgpackDecoder[Int] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.UnsignedInt(bytes) => runUnsignedInt(bytes, ctx)
        case MsgpackItem.SignedInt(bytes)   => runSignedInt(bytes, ctx)
        case x =>
          Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Int"))
      }
    }
  }

  implicit def listDecoder[A](implicit da: MsgpackDecoder[A]): MsgpackDecoder[List[A]] = new MsgpackDecoder[List[A]] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Array(size) if size < 0 =>
          Pull.raiseError(new MsgpackMalformedItemException(s"Negative array length ${size}"))
        case MsgpackItem.Array(size) if size <= Int.MaxValue => runList[F, A](size.toInt, ctx)
        case MsgpackItem.Array(size) =>
          Pull.raiseError(new MsgpackMalformedItemException("Array size exceeds Int.MaxValue"))
        case x =>
          Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "List"))
      }
    }
  }

  implicit object longDecoder extends MsgpackDecoder[Long] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.UnsignedInt(bytes) => runUnsignedLong(bytes, ctx)
        case MsgpackItem.SignedInt(bytes)   => runSignedLong(bytes, ctx)
        case x =>
          Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Long"))
      }
    }
  }

  implicit def mapDecoder[K, V](implicit pk: MsgpackDecoder[K], pv: MsgpackDecoder[V]): MsgpackDecoder[Map[K, V]] =
    new MsgpackDecoder[Map[K, V]] {
      def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
        item match {
          case MsgpackItem.Map(size) if size < 0 =>
            Pull.raiseError(new MsgpackMalformedItemException(s"Negative map length ${size}"))
          case MsgpackItem.Map(size) if size <= Int.MaxValue => runMap[F, K, V](size.toInt, ctx)
          case MsgpackItem.Map(size) =>
            Pull.raiseError(new MsgpackMalformedItemException("Map size exceeds Int.MaxValue"))
          case x =>
            Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Map"))
        }
      }
    }

  implicit object nilDecoder extends MsgpackDecoder[Null] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Nil => ctx.proceed(null)
        case x =>
          Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Null"))
      }
    }
  }

  implicit def optionDecoder[A](implicit da: MsgpackDecoder[A]): MsgpackDecoder[Option[A]] =
    new MsgpackDecoder[Option[A]] {
      def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
        if (item == MsgpackItem.Nil)
          ctx.proceed(None)
        else
          da.run(ctx).map { case (item, ctx) => (Some(item), ctx) }
      }
    }

  implicit object shortDecoder extends MsgpackDecoder[Short] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.UnsignedInt(bytes) => runUnsignedShort(bytes, ctx)
        case MsgpackItem.SignedInt(bytes)   => runSignedShort(bytes, ctx)
        case x =>
          Pull.raiseError(new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "Short"))
      }
    }
  }

  implicit object stringDecoder extends MsgpackDecoder[String] {
    def run[F[_]: RaiseThrowable](ctx: DecodingContext[F]) = get1(ctx) { (item, ctx) =>
      item match {
        case MsgpackItem.Str(bytes) => runString(bytes, ctx)
        case x =>
          Pull.raiseError(
            new MsgpackDecodingTypeMismatchException(s"MsgpackItem.${x.getClass.getSimpleName}", "String"))
      }
    }
  }
}
