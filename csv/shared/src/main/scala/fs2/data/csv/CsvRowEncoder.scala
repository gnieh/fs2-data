package fs2.data.csv

import cats.ContravariantMonoidal

trait CsvRowEncoder[T] {
  def headers: List[String]
  def encoder: RowEncoder[T]
}

object CsvRowEncoder {
  def apply[T](implicit ev: CsvRowEncoder[T]): CsvRowEncoder[T] = ev

  implicit val csvRowEncoderInstances = new ContravariantMonoidal[CsvRowEncoder] {

    override def product[A, B](fa: CsvRowEncoder[A], fb: CsvRowEncoder[B]): CsvRowEncoder[(A, B)] =
      new CsvRowEncoder[(A, B)] {
        override def headers: List[String] =
          fa.headers ++ fb.headers.toList

        override def encoder: RowEncoder[(A, B)] =
          ContravariantMonoidal[RowEncoder].product(fa.encoder, fb.encoder)

      }

    override def contramap[A, B](fa: CsvRowEncoder[A])(f: B => A): CsvRowEncoder[B] =
      new CsvRowEncoder[B] {

        override def headers: List[String] = fa.headers

        override def encoder: RowEncoder[B] = fa.encoder.contramap(f)

      }

    override def unit: CsvRowEncoder[Unit] =
      new CsvRowEncoder[Unit] {
        def headers = Nil
        def encoder = ContravariantMonoidal[RowEncoder].unit
      }


  }
}
