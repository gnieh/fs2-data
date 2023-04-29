package fs2.data.json.jq

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.parse.{Accumulator, Appender, Numbers, Parser => P, Parser0}
import cats.syntax.all._

case class JqParserException(error: P.Error) extends Exception(error.show)

object JqParser {

  private implicit object filterAcc extends Accumulator[Filter, Filter] {

    override def newAppender(first: Filter): Appender[Filter, Filter] =
      new Appender[Filter, Filter] {
        val bldr = List.newBuilder[Filter]
        def append(item: Filter) = {
          bldr += item
          this
        }

        def finish() = {
          val tail = bldr.result()
          if (tail.isEmpty)
            first
          else
            Jq.Sequence(NonEmptyList(first, tail))
        }
      }

  }

  private val whitespace: P[Char] = P.charIn(" \t\r\n")
  private val whitespace0: Parser0[Unit] = whitespace.rep0.void

  private val identifier: P[String] =
    (P.charIn(('a' to 'z') ++ ('A' to 'Z')) ~ P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "-_").rep0)
      .withContext("identifier")
      .string <* whitespace0

  private def kw(kw: String): P[Unit] =
    identifier.filter(_ == kw).void

  private def ch(c: Char): P[Unit] =
    P.char(c) <* whitespace0

  private def str(s: String): P[Unit] =
    P.string(s) <* whitespace0

  private val string: P[String] =
    P.char('"') *> P
      .oneOf(
        P.charsWhile(c => c != '"' && c != '\\') ::
          (P.char('\\') *> P.fromCharMap(Map('"' -> "\"", '\\' -> "\\"))) ::
          Nil)
      .repAs0[String] <* ch('"')

  private val index: P[Int] =
    Numbers.nonNegativeIntString.mapFilter(_.toIntOption) <* whitespace0

  private val filter: P[Filter] = {
    val access: P[Filter] =
      P.oneOf(
        string.map(Jq.Field(_)) ::
          (index ~ (ch(':') *> index.?).?)
            .collect {
              case (idx, None) =>
                Jq.Index(idx)
              case (idx1, Some(Some(idx2))) if idx1 == idx2 =>
                Jq.Index(idx1)
              case (min, Some(max)) if max.forall(min < _) =>
                Jq.Slice(min, max)
            } ::
          (ch(':') *> index.map(max => Jq.Slice(0, Some(max)))) ::
          Nil)
        .withContext("string, index, slice 'min:max' (with min <= max), slice 'idx:', or slice ':idx'")

    val step: P[Filter] =
      (P.char('.') *> P.oneOf(
        identifier.map(Jq.Field(_)) ::
          access.between(ch('['), ch(']')) ::
          Nil)).repAs[Filter].backtrack

    P.oneOf(
      str("..").as(Jq.RecursiveDescent) ::
        step ::
        ch('.').as(Jq.Identity) ::
        Nil)
      // repSepAs would be great here
      .repSep(ch('|'))
      .map {
        case NonEmptyList(filter, Nil) => filter
        case steps                     => Jq.Sequence(steps)
      }
  }

  private val query: P[Jq] = P.recursive[Jq] { query =>
    val constructor: P[Constructor] =
      P.oneOf(
        query.repSep0(ch(',')).with1.between(ch('['), ch(']')).map(Jq.Arr(_)) ::
        (string ~ (ch(':') *> query)).repSep0(ch(',')).with1.between(ch('{'), ch('}')).map(Jq.Obj(_)) ::
          string.map(Jq.Str(_)) ::
          kw("true").as(Jq.Bool(true)) ::
          kw("false").as(Jq.Bool(false)) ::
          kw("null").as(Jq.Null) ::
          Numbers.jsonNumber.map(Jq.Num(_)) ::
          Nil)

    whitespace0.with1 *>
      P.oneOf(
        (filter ~ (ch('|') *> constructor).?).map {
          case (filter, Some(cst)) =>
            Jq.Iterator(filter, cst)
          case (filter, None)      => filter
        } ::
          constructor ::
          Nil)
  }

  def parse[F[_]](input: String)(implicit F: MonadThrow[F]): F[Jq] =
    either(input).liftTo[F]

  def either(input: String): Either[Throwable, Jq] =
    (query <* P.end).parseAll(input).leftMap(JqParserException(_))

}
