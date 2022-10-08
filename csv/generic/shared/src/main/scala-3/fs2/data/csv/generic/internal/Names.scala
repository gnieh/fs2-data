package fs2.data.csv.generic
package internal

import shapeless3.deriving.*

private[generic] trait Names[T] {
  def names: List[String]
}

private[generic] object Names {
  given [T](using labels: Labelling[T], annotations: Annotations[CsvName, T]): Names[T] = new Names[T] {
    override def names: List[String] = {
      val annos = annotations.apply().toList.asInstanceOf[List[Option[CsvName]]]
      val names = labels.elemLabels.toList
      annos.zip(names).map(_.map(_.name).getOrElse(_))
    }
  }
}
