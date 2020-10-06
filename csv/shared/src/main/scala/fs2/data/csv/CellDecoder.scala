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

import java.net.{MalformedURLException, URI}
import java.time._
import java.time.format.DateTimeParseException
import java.util.UUID

import cats._
import cats.implicits._

import scala.util.Try
import scala.annotation.{implicitNotFound, tailrec}
import scala.concurrent.duration.{Duration, FiniteDuration}

/** Describes how a cell can be decoded to the given type.
  *
  * `CellDecoder` provides convenient methods such as `map`, `emap`, or `flatMap`
  * to build new decoders out of more basic one.
  *
  * Actually, `CellDecoder` has a [[https://typelevel.org/cats/api/cats/MonadError.html cats `MonadError`]]
  * instance. To get the full power of it, import `cats.implicits._`.
  */
@implicitNotFound(
  "No implicit CellDecoder found for type ${T}.\nYou can define one using CellDecoder.instance, by calling map on another CellDecoder or by using generic derivation for coproducts and unary products.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val cellDecoder: CellDecoder[${T}] = deriveCellDecoder\n\n")
trait CellDecoder[T] {
  def apply(cell: String): DecoderResult[T]

  /**
    * Map the parsed value.
    * @param f the mapping function
    * @tparam T2 the result type
    * @return a cell decoder reading the mapped type
    */
  def map[T2](f: T => T2): CellDecoder[T2] =
    s => apply(s).map(f)

  /**
    * Map the parsed value to a new decoder, which in turn will be applied to
    * the parsed value.
    * @param f the mapping function
    * @tparam T2 the result type
    * @return a cell decoder reading the mapped type
    */
  def flatMap[T2](f: T => CellDecoder[T2]): CellDecoder[T2] =
    s => apply(s).flatMap(f(_)(s))

  /**
    * Map the parsed value, potentially failing.
    * @param f the mapping function
    * @tparam T2 the result type
    * @return a cell decoder reading the mapped type
    */
  def emap[T2](f: T => DecoderResult[T2]): CellDecoder[T2] =
    s => apply(s).flatMap(f)

  /**
    * Fail-over. If this decoder fails, try the supplied other decoder.
    * @param cd the fail-over decoder
    * @tparam TT the return type
    * @return a decoder combining this and the other decoder
    */
  def or[TT >: T](cd: => CellDecoder[TT]): CellDecoder[TT] =
    s =>
      apply(s) match {
        case Left(_)      => cd(s)
        case r @ Right(_) => r.leftCast[DecoderError]
      }

  /**
    * Similar to [[or]], but return the result as an Either signaling which cell decoder succeeded. Allows for parsing
    * an unrelated type in case of failure.
    * @param cd the alternative decoder
    * @tparam B the type the alternative decoder returns
    * @return a decoder combining both decoders
    */
  def either[B](cd: CellDecoder[B]): CellDecoder[Either[T, B]] =
    s =>
      apply(s) match {
        case Left(_) =>
          cd(s) match {
            case l @ Left(_)  => l.rightCast[Either[T, B]]
            case r @ Right(_) => r.leftCast[T].asRight
          }
        case Right(value) => Right(Left(value))
      }
}

object CellDecoder
    extends CellDecoderInstances1
    with LiteralCellDecoders
    with ExportedCellDecoders
    with PlatformCellDecoders {

  implicit object CellDecoderInstances extends MonadError[CellDecoder, DecoderError] with SemigroupK[CellDecoder] {

    override def map[A, B](fa: CellDecoder[A])(f: A => B): CellDecoder[B] =
      fa.map(f)

    def flatMap[A, B](fa: CellDecoder[A])(f: A => CellDecoder[B]): CellDecoder[B] =
      fa.flatMap(f)

    def handleErrorWith[A](fa: CellDecoder[A])(f: DecoderError => CellDecoder[A]): CellDecoder[A] =
      s => fa(s).leftFlatMap(f(_)(s))

    def pure[A](x: A): CellDecoder[A] =
      _ => Right(x)

    def raiseError[A](e: DecoderError): CellDecoder[A] =
      _ => Left(e)

    def tailRecM[A, B](a: A)(f: A => CellDecoder[Either[A, B]]): CellDecoder[B] = {
      @tailrec
      def step(s: String, a: A): DecoderResult[B] =
        f(a)(s) match {
          case left @ Left(_)          => left.rightCast[B]
          case Right(Left(a))          => step(s, a)
          case Right(right @ Right(_)) => right.leftCast[DecoderError]
        }
      s => step(s, a)
    }

    override def combineK[A](x: CellDecoder[A], y: CellDecoder[A]): CellDecoder[A] = x or y
  }

  def apply[T: CellDecoder]: CellDecoder[T] = implicitly[CellDecoder[T]]

  @inline
  def instance[T](f: String => DecoderResult[T]): CellDecoder[T] = s => f(s)
  @inline
  def fromString[T](f: String => T): CellDecoder[T] = s => f(s).asRight

  def enumerationDecoder[E <: Enumeration](enum: E): CellDecoder[E#Value] =
    s =>
      enum.values
        .find(_.toString === s)
        .toRight(new DecoderError(s"unable to decode '$s' as a ${enum.toString()} value"))

  // Primitives
  implicit val unitDecoder: CellDecoder[Unit] = s =>
    if (s.isEmpty)
      Right(())
    else
      Left(new DecoderError(s"'$s' can't be decoded as unit as it is not empty"))
  implicit val booleanDecoder: CellDecoder[Boolean] = s =>
    Either.catchNonFatal(s.toBoolean).leftMap(new DecoderError(s"unable to decode '$s' as a byte", _))
  implicit val byteDecoder: CellDecoder[Byte] = s =>
    Either.catchNonFatal(s.toByte).leftMap(new DecoderError(s"unable to decode '$s' as a byte", _))
  implicit val shortDecoder: CellDecoder[Short] = s =>
    Either.catchNonFatal(s.toShort).leftMap(new DecoderError(s"unable to decode '$s' as a short", _))
  implicit val charDecoder: CellDecoder[Char] = s =>
    if (s.length == 1)
      Right(s(0))
    else
      Left(new DecoderError(s"unable to decode '$s' as a character"))
  implicit val intDecoder: CellDecoder[Int] = s =>
    Either.catchNonFatal(s.toInt).leftMap(new DecoderError(s"unable to decode '$s' as an integer", _))
  implicit val longDecoder: CellDecoder[Long] = s =>
    Either.catchNonFatal(s.toLong).leftMap(new DecoderError(s"unable to decode '$s' as a long", _))
  implicit val floatDecoder: CellDecoder[Float] = s =>
    Either.catchNonFatal(s.toFloat).leftMap(new DecoderError(s"unable to decode '$s' as a float", _))
  implicit val doubleDecoder: CellDecoder[Double] = s =>
    Either.catchNonFatal(s.toDouble).leftMap(new DecoderError(s"unable to decode '$s' as a double", _))
  implicit val bigDecimalDecoder: CellDecoder[BigDecimal] = s =>
    Try(BigDecimal(s)).toEither.leftMap(new DecoderError(s"unable to decode '$s' as a BigDecimal", _))
  implicit val bigIntDecoder: CellDecoder[BigInt] = s =>
    Try(BigInt(s)).toEither.leftMap(new DecoderError(s"unable to decode '$s' as a BigInt", _))
  implicit val stringDecoder: CellDecoder[String] = s => Right(s)
  implicit val charArrayDecoder: CellDecoder[Array[Char]] = s => Right(s.toCharArray)

  // Containers
  implicit def decoderResultDecoder[T](implicit ev: CellDecoder[T]): CellDecoder[DecoderResult[T]] = s => ev(s).asRight
  implicit def rawOrResultDecoder[T](implicit ev: CellDecoder[T]): CellDecoder[Either[String, T]] =
    s => ev(s).leftMap(_ => s).asRight

  // Standard Library types
  implicit val finiteDurationDecoder: CellDecoder[FiniteDuration] =
    durationDecoder.emap {
      case fd: FiniteDuration => fd.asRight
      case d                  => new DecoderError(s"parsed duration isn't finite: $d").asLeft
    }

  implicit override val javaUriDecoder: CellDecoder[URI] = s =>
    Either
      .catchOnly[MalformedURLException](new URI(s))
      .leftMap(t => new DecoderError(s"couldn't parse URI", t))

  implicit val uuidDecoder: CellDecoder[UUID] = s =>
    Either
      .catchOnly[IllegalArgumentException](UUID.fromString(s))
      .leftMap(t => new DecoderError(s"couldn't parse UUID", t))

  // Java Time
  implicit val instantDecoder: CellDecoder[Instant] = javaTimeDecoder("Instant", Instant.parse)
  implicit val periodDecoder: CellDecoder[Period] = javaTimeDecoder("Period", Period.parse)
  implicit val localDateDecoder: CellDecoder[LocalDate] = javaTimeDecoder("LocalDate", LocalDate.parse)
  implicit val localDateTimeDecoder: CellDecoder[LocalDateTime] = javaTimeDecoder("LocalDateTime", LocalDateTime.parse)
  implicit val localTimeDecoder: CellDecoder[LocalTime] = javaTimeDecoder("LocalTime", LocalTime.parse)
  implicit val offsetDateTimeDecoder: CellDecoder[OffsetDateTime] =
    javaTimeDecoder("OffsetDateTime", OffsetDateTime.parse)
  implicit val offsetTimeDecoder: CellDecoder[OffsetTime] = javaTimeDecoder("OffsetTime", OffsetTime.parse)
  implicit val zonedDateTimeDecoder: CellDecoder[ZonedDateTime] = javaTimeDecoder("ZonedDateTime", ZonedDateTime.parse)

  private def javaTimeDecoder[T](name: String, t: String => T): CellDecoder[T] =
    s =>
      Either
        .catchOnly[DateTimeParseException](t(s))
        .leftMap(t => new DecoderError(s"couldn't parse $name", t))
}

trait CellDecoderInstances1 {
  implicit val durationDecoder: CellDecoder[Duration] = s =>
    Either
      .catchNonFatal(Duration(s))
      .leftMap(t => new DecoderError(s"couldn't parse Duration", t))
}

trait ExportedCellDecoders {
  implicit def exportedCellDecoders[A](implicit exported: Exported[CellDecoder[A]]): CellDecoder[A] = exported.instance
}
