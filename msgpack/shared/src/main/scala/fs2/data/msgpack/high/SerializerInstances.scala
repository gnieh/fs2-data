package fs2
package data
package msgpack
package high

import scodec.bits.*

import low.MsgpackItem
import scala.annotation.tailrec

trait SerializerInstances {
  def right1(x: MsgpackItem) = Right(Chunk(x))

  private def countBytes[A](x: A, f: A => A): Int = {
    @tailrec
    def go(current: A, bytes: Int): Int =
      if (current == 0)
        bytes
      else
        go(f(current), bytes + 1)

    if (x == 0)
      1
    else
      go(x, 0)
  }

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
      val nbytes = countBytes[Int](x, _ >>> 8)
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
      val nbytes = countBytes[Long](x, _ >>> 8)
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
    val header = Chunk(MsgpackItem.Map(map.size.toLong))

    map.foldLeft(Right(header): SerializationResult) { case (result, (k, v)) =>
      for {
        acc <- result
        key <- sk(k)
        value <- sv(v)
      } yield acc ++ key ++ value
    }
  }

  @inline implicit def listSerializer[A](implicit sa: MsgpackSerializer[A]): MsgpackSerializer[List[A]] = { list =>
    val header = Chunk(MsgpackItem.Array(list.size.toLong))

    list.foldLeft(Right(header): SerializationResult) { (result, x) =>
      for {
        acc <- result
        item <- sa(x)
      } yield acc ++ item
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
