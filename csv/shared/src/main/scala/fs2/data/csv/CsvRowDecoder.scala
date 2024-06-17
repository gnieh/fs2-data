package fs2.data.csv

import cats.syntax.all._
import cats._

trait CsvRowDecoder[T] {
  def apply(headers: List[String]): Either[DecoderError, RowDecoder[T]]
}

object CsvRowDecoder {
  def as[T: CellDecoder](name: String): CsvRowDecoder[T] = {
    (headers: List[String]) =>
      val i = headers.toList.indexOf(name)
      Either.cond(
        i >= 0,
        (row: Row) =>
          row.values.toList.lift(i).toRight {
            new HeaderSizeError(headers.size, row.values.size, row.line)
          }.flatMap(CellDecoder[T].apply),
        new DecoderError(s"unknown field $name")
      )
  }

  implicit def decodeResultCsvRowDecoder[T](implicit
      dec: CsvRowDecoder[T]): CsvRowDecoder[DecoderResult[T]] =
    new CsvRowDecoder[DecoderResult[T]] {
      override def apply(headers: List[String]): Either[DecoderError,RowDecoder[DecoderResult[T]]] =
        dec(headers).map(_.map(_.asRight))
    }

  implicit val CsvRowDecoderInstances: Applicative[CsvRowDecoder] with SemigroupK[CsvRowDecoder] =
    new Applicative[CsvRowDecoder] with SemigroupK[CsvRowDecoder] {

      override def combineK[A](x: CsvRowDecoder[A], y: CsvRowDecoder[A]): CsvRowDecoder[A] =
        headers =>
          (x(headers), y(headers)).mapN(SemigroupK[RowDecoder].combineK)

      override def map[A, B](fa: CsvRowDecoder[A])(f: A => B): CsvRowDecoder[B] =
        headers => fa(headers).map(_.map(f))

      def pure[A](x: A): CsvRowDecoder[A] =
        _ => Right(x.pure[RowDecoder])
      
      def ap[A, B](ff: CsvRowDecoder[A => B])(fa: CsvRowDecoder[A]): CsvRowDecoder[B] =
        headers =>
          (ff(headers), fa(headers)).mapN((a, b) => Applicative[RowDecoder].ap(a)(b))
    }
}
