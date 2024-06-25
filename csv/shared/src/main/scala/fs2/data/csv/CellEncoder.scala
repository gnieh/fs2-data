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

package fs2.data.csv

import java.net.URI
import java.util.UUID

import cats._
import cats.syntax.all._

import scala.annotation.implicitNotFound
import scala.concurrent.duration.{Duration, FiniteDuration}

/** Describes how a cell of a given type can be encoded.
  */
@implicitNotFound(
  "No implicit CellEncoder found for type ${T}.\nYou can define one using CellEncoder.instance, by calling contramap on another CellEncoder or by using generic derivation for coproducts and unary products.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val cellEncoder: CellEncoder[${T}] = deriveCellEncoder\n\n")
@FunctionalInterface trait CellEncoder[T] {
  def apply(cell: T): String

  def contramap[B](f: B => T): CellEncoder[B] = (cell: B) => apply(f(cell))
}

object CellEncoder
    extends CellEncoderInstances1
    with CellEncoderInstances2
    with LiteralCellEncoders
    with EnumEncoders
    with ExportedCellEncoders
    with PlatformCellEncoders {

  implicit object CellEncoderInstances extends Contravariant[CellEncoder] {
    override def contramap[A, B](fa: CellEncoder[A])(f: B => A): CellEncoder[B] = fa.contramap(f)
  }

  def apply[T: CellEncoder]: CellEncoder[T] = implicitly[CellEncoder[T]]

  @inline
  def instance[T](f: T => String): CellEncoder[T] = s => f(s)

  @inline
  def const[T](r: String): CellEncoder[T] = _ => r

  @inline
  def fromToString[A]: CellEncoder[A] = _.toString

  @inline
  def fromShow[A](implicit ev: Show[A]): CellEncoder[A] = instance(ev.show)

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
  implicit val stringEncoder: CellEncoder[String] = new CellEncoder[String] {
    override def apply(cell: String): String = cell
  }
  implicit val charArrayEncoder: CellEncoder[Array[Char]] = new String(_)

  // Standard Library types
  implicit val finiteDurationEncoder: CellEncoder[FiniteDuration] =
    durationEncoder.narrow
  implicit val javaUriEncoder: CellEncoder[URI] = fromToString(_)
  implicit val uuidEncoder: CellEncoder[UUID] = fromToString(_)

  // Option
  implicit def optionEncoder[Cell: CellEncoder]: CellEncoder[Option[Cell]] =
    _.fold("")(CellEncoder[Cell].apply)

}

trait CellEncoderInstances1 {
  implicit val durationEncoder: CellEncoder[Duration] = _.toString
}

// Java Time Encoders
trait CellEncoderInstances2 {
  import java.time._
  import java.time.format.DateTimeFormatter
  import java.time.temporal.TemporalAccessor

  implicit val instantEncoder: CellEncoder[Instant] = _.toString
  implicit val periodEncoder: CellEncoder[Period] = _.toString
  implicit def localDateEncoder(implicit
      localDateEncodeFmt: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE): CellEncoder[LocalDate] =
    javaTimeEncoderWithFmt(_)(localDateEncodeFmt)
  implicit def localDateTimeEncoder(implicit
      localDateTimeEncodeFmt: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME): CellEncoder[LocalDateTime] =
    javaTimeEncoderWithFmt(_)(localDateTimeEncodeFmt)
  implicit def localTimeEncoder(implicit
      localTimeEncodeFmt: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME): CellEncoder[LocalTime] =
    javaTimeEncoderWithFmt(_)(localTimeEncodeFmt)
  implicit def offsetDateTimeEncoder(implicit
      offsetDateTimeEncodeFmt: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      : CellEncoder[OffsetDateTime] = javaTimeEncoderWithFmt(_)(offsetDateTimeEncodeFmt)
  implicit def offsetTimeEncoder(implicit
      offsetTimeEncodeFmt: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME): CellEncoder[OffsetTime] =
    javaTimeEncoderWithFmt(_)(offsetTimeEncodeFmt)
  implicit def zonedDateTimeEncoder(implicit
      zonedDateTimeEncodeFmt: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME): CellEncoder[ZonedDateTime] =
    javaTimeEncoderWithFmt(_)(zonedDateTimeEncodeFmt)
  implicit val dayOfWeekEncoder: CellEncoder[DayOfWeek] = _.toString
  implicit val javaTimeDurationEncoder: CellEncoder[java.time.Duration] = _.toString
  implicit val monthEncoder: CellEncoder[Month] = _.toString
  implicit val monthDayEncoder: CellEncoder[MonthDay] = _.toString
  implicit val yearDayEncoder: CellEncoder[Year] = _.toString
  implicit val yearMonthDayEncoder: CellEncoder[YearMonth] = _.toString
  implicit val zoneIdEncoder: CellEncoder[ZoneId] = _.toString
  implicit val zoneOffsetEncoder: CellEncoder[ZoneOffset] = _.toString

  private def javaTimeEncoderWithFmt[T <: TemporalAccessor](value: T)(implicit fmt: DateTimeFormatter): String = {
    fmt.format(value)
  }
}

trait ExportedCellEncoders {
  implicit def exportedCellEncoders[A](implicit exported: Exported[CellEncoder[A]]): CellEncoder[A] =
    exported.instance
}
