package fs2.data.csv

import cats.data.NonEmptyList

private[csv] sealed trait HeaderContainer {
  type Header
  type Self <: HeaderContainer
  def findHeader(header: Header): Option[Int]
  def delete(header: Header): Option[Self]
  def toList: List[Header]
}

object HeaderContainer {
  // The Aux is only here to help scalac 2.12, on 2.13+ HeaderContainer could just have a type param
  type Aux[H] = HeaderContainer { type Header = H }
}

class NoHeaders[I] private[csv] (length: Int)(implicit ev1: I <:< Int) extends HeaderContainer {
  override type Header = I
  override type Self = NoHeaders[Int]
  override def findHeader(header: I): Option[Int] = if (header >= 0 && header < length) Some(header) else None
  override def delete(header: Header): Option[Self] = if (length > 0) Some(new NoHeaders(length - 1)) else None
  override def toList: List[Header] = List.tabulate(length)(i => i.asInstanceOf[I])
}

class NelHeaders[H] private[csv] (private[csv] val headers: NonEmptyList[H]) extends HeaderContainer {
  override type Header = H
  override type Self = NelHeaders[Header]
  override def findHeader(header: Header): Option[Int] = {
    val idx = headers.toList.indexOf(header)
    if (idx >= 0) Some(idx) else None
  }
  override def delete(header: Header): Option[Self] = headers.toList.span(_ != header) match {
    case (_, Nil)                  => Some(this)
    case (Nil, _ :: Nil)           => None
    case (Nil, _ :: first :: tail) => Some(new NelHeaders(NonEmptyList(first, tail)).asInstanceOf)
    case (first :: pre, _ :: tail) => Some(new NelHeaders(NonEmptyList(first, pre ::: tail)).asInstanceOf)
  }
  override def toList: List[Header] = headers.toList
}
