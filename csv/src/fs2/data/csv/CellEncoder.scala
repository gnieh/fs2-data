/*
 * Copyright 2019 Lucas Satabin
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
package fs2.data.csv

import java.net.URL
import java.time._
import java.util.UUID

import cats._
import cats.implicits._

import scala.concurrent.duration.{Duration, FiniteDuration}

/** Describes how a cell of a given type can be encoded.
  */
trait CellEncoder[T] {
  def apply(cell: T): String

  def contramap[B](f: B => T): CellEncoder[B] = (cell: B) => apply(f(cell))
}

object CellEncoder extends CellEncoderInstances1 with LiteralCellEncoders with ExportedCellEncoders {

  implicit object CellEncoderInstances extends Contravariant[CellEncoder] {
    override def contramap[A, B](fa: CellEncoder[A])(f: B => A): CellEncoder[B] = fa.contramap(f)
  }

  def apply[T: CellEncoder]: CellEncoder[T] = implicitly[CellEncoder[T]]

  @inline
  def instance[T](f: T => String): CellEncoder[T] = s => f(s)

  @inline
  def fromToString[A]: CellEncoder[A] = _.toString

  implicit def enumerationEncoder[E <: Enumeration]: CellEncoder[E#Value] =
    _.toString

  // Primitives
  implicit val unitEncoder: CellEncoder[Unit] = _ => ""
  implicit val booleanEncoder: CellEncoder[Boolean] = fromToString(_)
  implicit val byteEncoder: CellEncoder[Byte] = fromToString(_)
  implicit val shortEncoder: CellEncoder[Short] = fromToString(_)
  implicit val charEncoder: CellEncoder[Char] = fromToString(_)
  implicit val intEncoder: CellEncoder[Int] = fromToString(_)
  implicit val longEncoder: CellEncoder[Long] = fromToString(_)
  implicit val floatEncoder: CellEncoder[Float] = fromToString(_)
  implicit val doubleEncoder: CellEncoder[Double] = fromToString(_)
  implicit val bigDecimalEncoder: CellEncoder[BigDecimal] = fromToString(_)
  implicit val bigIntEncoder: CellEncoder[BigInt] = fromToString(_)
  implicit val stringEncoder: CellEncoder[String] = identity
  implicit val charArrayEncoder: CellEncoder[Array[Char]] = new String(_)

  // Standard Library types
  implicit val finiteDurationEncoder: CellEncoder[FiniteDuration] =
    durationEncoder.narrow
  implicit val javaUrlEncoder: CellEncoder[URL] = fromToString(_)
  implicit val uuidEncoder: CellEncoder[UUID] = fromToString(_)

  // Java Time
  implicit val instantEncoder: CellEncoder[Instant] = fromToString(_)
  implicit val periodEncoder: CellEncoder[Period] = fromToString(_)
  implicit val localDateEncoder: CellEncoder[LocalDate] = fromToString(_)
  implicit val localDateTimeEncoder: CellEncoder[LocalDateTime] = fromToString(_)
  implicit val localTimeEncoder: CellEncoder[LocalTime] = fromToString(_)
  implicit val offsetDateTimeEncoder: CellEncoder[OffsetDateTime] =
    fromToString(_)
  implicit val offsetTimeEncoder: CellEncoder[OffsetTime] = fromToString(_)
  implicit val zonedDateTimeEncoder: CellEncoder[ZonedDateTime] = fromToString(_)

  // Option
  implicit def optionEncoder[Cell: CellEncoder]: CellEncoder[Option[Cell]] =
    _.fold("")(CellEncoder[Cell].apply)

}

trait CellEncoderInstances1 {
  implicit val durationEncoder: CellEncoder[Duration] = _.toString
}

trait ExportedCellEncoders {
  implicit def exportedCellEncoders[A](implicit exported: Exported[CellEncoder[A]]): CellEncoder[A] =
    exported.instance
}
