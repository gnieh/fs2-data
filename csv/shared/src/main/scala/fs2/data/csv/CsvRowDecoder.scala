package fs2.data.csv

import cats.syntax.all._
import cats._

sealed trait CsvRowDecoder[T] {
  def apply(headers: List[String]): Either[DecoderError, RowDecoder[T]]
}

object CsvRowDecoder {
  def as[T: CellDecoder](name: String): CsvRowDecoder[T] = new CsvRowDecoder[T] {
    def apply(headers: List[String]) = {
      val i = headers.toList.indexOf(name)
      Either.cond(
        i >= 0,
        RowDecoder.instance { (row: Row) =>
          row.values.toList.lift(i).toRight {
            new HeaderSizeError(headers.size, row.values.size, row.line)
          }.flatMap(CellDecoder[T].apply)
        },
        new DecoderError(s"unknown field $name")
      )
    }
  }

  private[csv] def instance[T](f: List[String] => Either[DecoderError, RowDecoder[T]]): CsvRowDecoder[T] = new CsvRowDecoder[T] {
    def apply(headers: List[String]) = f(headers)
  }

  implicit def decodeResultCsvRowDecoder[T](implicit
      dec: CsvRowDecoder[T]): CsvRowDecoder[DecoderResult[T]] =
    new CsvRowDecoder[DecoderResult[T]] {
      override def apply(headers: List[String]): Either[DecoderError,RowDecoder[DecoderResult[T]]] =
        dec(headers).map(_.map(_.asRight))
    }

  implicit val CsvRowDecoderInstances: Applicative[CsvRowDecoder] with SemigroupK[CsvRowDecoder] =
    new Applicative[CsvRowDecoder] with SemigroupK[CsvRowDecoder] {
      val a = Applicative[List[String] => *].compose(Applicative[Either[DecoderError, *]].compose(Applicative[RowDecoder]))
      override def combineK[A](x: CsvRowDecoder[A], y: CsvRowDecoder[A]): CsvRowDecoder[A] =
        instance((x.apply, y.apply).mapN(_ <+> _))
        
      override def map[A, B](fa: CsvRowDecoder[A])(f: A => B): CsvRowDecoder[B] =
        instance(a.map(fa.apply)(f))

      def pure[A](x: A): CsvRowDecoder[A] =
        instance(a.pure(x))
      
      def ap[A, B](ff: CsvRowDecoder[A => B])(fa: CsvRowDecoder[A]): CsvRowDecoder[B] = {
        instance(a.ap(ff.apply)(fa.apply))
      }
    }
}
