package fs2.data.json.jq

import cats.MonadThrow
import cats.data.NonEmptyChain
import cats.parse.{Accumulator0, Appender, Numbers, Parser => P, Parser0}
import cats.syntax.all._
import cats.data.Chain
import cats.data.NonEmptyList

case class JqParserException(error: P.Error) extends Exception(error.show)

object JqParser {

  private implicit object filterAcc extends Accumulator0[Filter, Filter] {

    override def newAppender(): Appender[Filter, Filter] =
      new Appender[Filter, Filter] {
        val bldr = List.newBuilder[Filter]
        def append(item: Filter) = {
          bldr += item
          this
        }

        def finish() =
          bldr.result().filterNot(_ == Jq.Identity) match {
            case Nil      => Jq.Identity
            case f :: Nil => f
            case f :: fs  => Jq.Sequence(NonEmptyChain.fromChainPrepend(f, Chain.fromSeq(fs)))
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
        .between(ch('['), ch(']'))
        .withContext("string, index, slice 'min:max' (with min <= max), slice 'idx:', or slice ':idx'")

    val step: P[Filter] =
      (P.char('.') *> P
        .oneOf(
          identifier.map(Jq.Field(_)) ::
            access.backtrack ::
            Nil) ~ access.backtrack.repAs0[Filter])
        .map { case (fst, snd) =>
          fst ~ snd
        }
        .repAs[Filter]

    P.oneOf(
      (str("..") *> (access.backtrack ~ step.repAs0[Filter]).?).map {
        case Some((access, rest)) => Jq.Sequence(NonEmptyChain(Jq.RecursiveDescent, access, rest))
        case None                 => Jq.RecursiveDescent
      } ::
        step ::
        Nil)
      // repSepAs would be great here
      .repSep(ch('|'))
      .map {
        case NonEmptyList(filter, Nil) => filter
        case steps                     => Jq.Sequence(NonEmptyChain.fromNonEmptyList(steps))
      }
  }

  private val selector: P[(Filter, Jq => Jq)] = P.recursive[(Filter, Jq => Jq)] { selector =>
    (filter ~ (str("[]") *> selector.?).?).map {
      case (prefix, None) =>
        (prefix, identity)
      case (prefix, Some(None)) =>
        (Jq.Identity, Jq.Iterator(prefix, _))
      case (prefix1, Some(Some((prefix2, f)))) =>
        (prefix2, inner => Jq.Iterator(prefix1, f(inner)))
    }
  }

  private val query: P[Jq] = P.recursive[Jq] { query =>
    val constructor: P[Filter => Constructor] =
      P.oneOf(
        query
          .repSep0(ch(','))
          .with1
          .between(ch('['), ch(']'))
          .map[Filter => Constructor](fs => prefix => Jq.Arr(prefix, fs)) ::
          (string ~ (ch(':') *> query))
            .repSep0(ch(','))
            .with1
            .between(ch('{'), ch('}'))
            .map[Filter => Constructor](fs => prefix => Jq.Obj(prefix, fs)) ::
          string.map[Filter => Constructor](s => _ => Jq.Str(s)) ::
          kw("true").as[Filter => Constructor](_ => Jq.Bool(true)) ::
          kw("false").as[Filter => Constructor](_ => Jq.Bool(false)) ::
          kw("null").as[Filter => Constructor](_ => Jq.Null) ::
          Numbers.jsonNumber.map[Filter => Constructor](n => _ => Jq.Num(n)) ::
          Nil)

    whitespace0.with1 *>
      P.oneOf(
        (selector ~ (ch('|') *> constructor).?).map {
          case ((filter, f), Some(cst)) =>
            f(cst(filter))
          case ((filter, f), None) => f(filter)
        } ::
          constructor.map(cst => cst(Jq.Identity)) ::
          Nil)
  }

  def parse[F[_]](input: String)(implicit F: MonadThrow[F]): F[Jq] =
    either(input).liftTo[F]

  def either(input: String): Either[Throwable, Jq] =
    (query <* P.end).parseAll(input).leftMap(JqParserException(_))

}
