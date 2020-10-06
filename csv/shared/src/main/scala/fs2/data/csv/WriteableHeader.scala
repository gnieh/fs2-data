package fs2.data.csv

import cats.Contravariant
import cats.data.NonEmptyList

trait WriteableHeader[Header] {
  def apply(headers: NonEmptyList[Header]): NonEmptyList[String]

  def contramap[B](f: B => Header): WriteableHeader[B] = headers => apply(headers.map(f))
}

object WriteableHeader {
  def apply[Header: WriteableHeader]: WriteableHeader[Header] =
    implicitly[WriteableHeader[Header]]

  implicit object StringWriteableHeader extends WriteableHeader[String] {
    def apply(names: NonEmptyList[String]): NonEmptyList[String] = names
  }

  implicit object WriteableHeaderInstances extends Contravariant[WriteableHeader] {
    override def contramap[A, B](fa: WriteableHeader[A])(f: B => A): WriteableHeader[B] = fa.contramap(f)
  }

  def liftCellEncoder[T](implicit cellEncoder: CellEncoder[T]): WriteableHeader[T] =
    _.map(cellEncoder(_))
}
