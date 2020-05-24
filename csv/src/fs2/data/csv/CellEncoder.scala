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
}

object CellEncoder
    extends CellEncoderInstances1
    with LiteralCellEncoders
    with ExportedCellEncoders {

  implicit object CellEncoderInstances extends Contravariant[CellEncoder] {
    override def contramap[A, B](fa: CellEncoder[A])(
        f: B => A): CellEncoder[B] = (cell: B) => fa(f(cell))
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
  implicit val booleanEncoder: CellEncoder[Boolean] = _.toString
  implicit val byteEncoder: CellEncoder[Byte] = _.toString
  implicit val shortEncoder: CellEncoder[Short] = _.toString
  implicit val charEncoder: CellEncoder[Char] = _.toString
  implicit val intEncoder: CellEncoder[Int] = _.toString
  implicit val longEncoder: CellEncoder[Long] = _.toString
  implicit val floatEncoder: CellEncoder[Float] = _.toString
  implicit val doubleEncoder: CellEncoder[Double] = _.toString
  implicit val bigDecimalEncoder: CellEncoder[BigDecimal] = _.toString
  implicit val bigIntEncoder: CellEncoder[BigInt] = _.toString
  implicit val stringEncoder: CellEncoder[String] = identity
  implicit val charArrayEncoder: CellEncoder[Array[Char]] = new String(_)

  // Standard Library types
  implicit val finiteDurationEncoder: CellEncoder[FiniteDuration] =
    durationEncoder.narrow
  implicit val javaUrlEncoder: CellEncoder[URL] = _.toString
  implicit val uuidEncoder: CellEncoder[UUID] = _.toString

  // Java Time
  implicit val instantEncoder: CellEncoder[Instant] = _.toString
  implicit val periodEncoder: CellEncoder[Period] = _.toString
  implicit val localDateEncoder: CellEncoder[LocalDate] = _.toString
  implicit val localDateTimeEncoder: CellEncoder[LocalDateTime] = _.toString
  implicit val localTimeEncoder: CellEncoder[LocalTime] = _.toString
  implicit val offsetDateTimeEncoder: CellEncoder[OffsetDateTime] = _.toString
  implicit val offsetTimeEncoder: CellEncoder[OffsetTime] = _.toString
  implicit val zonedDateTimeEncoder: CellEncoder[ZonedDateTime] = _.toString

  // Option
  implicit def optionEncoder[Cell: CellEncoder]: CellEncoder[Option[Cell]] =
    _.fold("")(CellEncoder[Cell].apply)

}

trait CellEncoderInstances1 {
  implicit val durationEncoder: CellEncoder[Duration] = _.toString
}

trait ExportedCellEncoders {
  implicit def exportedCellEncoders[A](
      implicit exported: Exported[CellEncoder[A]]): CellEncoder[A] =
    exported.instance
}
