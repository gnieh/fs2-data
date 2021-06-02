package fs2.data.csv
package generic.internal

import cats.syntax.all._

sealed trait OptCellDecoder[T] {
  def apply(name: String, value: Option[String]): DecoderResult[T]
}

object OptCellDecoder extends LowPrioOptCellDecoders {
  given makeNonOpt[A: CellDecoder]: OptCellDecoder[A] = new OptCellDecoder[A] {
    override def apply(name: String, value: Option[String]): DecoderResult[A] =
      value.toRight(new DecoderError(s"Missing column $name")).flatMap(CellDecoder[A].apply)
  }
}

trait LowPrioOptCellDecoders {
  given makeOpt[A: CellDecoder]: OptCellDecoder[Option[A]] = new OptCellDecoder[Option[A]] {
    override def apply(name: String, value: Option[String]): DecoderResult[Option[A]] = value.traverse(CellDecoder[A].apply(_))
  }
}
