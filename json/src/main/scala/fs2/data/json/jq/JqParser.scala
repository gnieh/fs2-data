package fs2.data.json.jq

import cats.MonadError
import cats.data.StateT
import cats.syntax.all._
import scala.annotation.tailrec

case class JqParserException(expected: String, got: String, idx: Int)
    extends Exception(s"unexpected '$got' at index $idx, $expected was expected")

class JqParser[F[_]](val input: String)(implicit F: MonadError[F, Throwable]) {

  private type Parser[T] = StateT[F, Int, T]

  private def idx: Parser[Int] =
    StateT.get

  private def modify(f: Int => Int): Parser[Unit] =
    StateT.modify(f)

  private def pure[T](v: T): Parser[T] =
    StateT.pure(v)

  private def tailRecM[A, B](init: A)(f: A => Parser[Either[A, B]]): Parser[B] =
    init.tailRecM(f)

  private def raiseSyntaxError[T](expected: String, got: String): Parser[T] =
    idx.flatMapF(JqParserException(expected, got, _).raiseError)

  private def accept(c: Char): Parser[Unit] =
    peek.flatMap {
      case Some(`c`)   => consume
      case Some(other) => raiseSyntaxError(c.toString(), other.toString())
      case None        => raiseSyntaxError(c.toString(), "<eos>")
    }

  private val peek: Parser[Option[Char]] =
    idx.map(idx =>
      if (idx >= input.size)
        none
      else
        input.charAt(idx).some)

  private val consume: Parser[Unit] =
    modify(_ + 1)

  def readWhile(pred: Char => Boolean): Parser[String] =
    idx.flatMap { idx =>
      @tailrec
      def loop(idx: Int, builder: StringBuilder): Parser[String] =
        if (idx >= input.size || !pred(input.charAt(idx)))
          modify(_ => idx).as(builder.result())
        else
          loop(idx + 1, builder.append(input.charAt(idx)))
      loop(idx, new StringBuilder)
    }

  def parse(): F[Jq] =
    F.raiseError(JqParserException("something", "nothing", 0))

}

private sealed trait JqToken
private object JqToken {
  case object Dot extends JqToken
  case object Pipe extends JqToken
  case object RecursiveDescent extends JqToken
  case object Del extends JqToken
  case object LParen extends JqToken
  case object RParen extends JqToken
  case object LBracket extends JqToken
  case object RBracket extends JqToken
  case object LBrace extends JqToken
  case object RBrace extends JqToken
  case object Comma extends JqToken

  case class Str(s: String) extends JqToken
  case class Ident(id: String) extends JqToken
  case class Integer(i: Int) extends JqToken
  case class Number(n: BigDecimal) extends JqToken
  case object True extends JqToken
  case object False extends JqToken
  case object Null extends JqToken
}
