package fs2.data.csv.generic

import cats.implicits._
import fs2.data.csv.{CellDecoder, DecoderResult}
import org.scalatest.{EitherValues, FlatSpec, Matchers}
import shapeless.Annotation

sealed trait Simple
case object On extends Simple
case object Off extends Simple

sealed trait Complex
case object Active extends Complex
object Inactive extends Complex
case class Numbered(n: Int) extends Complex
case class Unknown(state: String) extends Complex

sealed trait Alphabet
@CsvValue("A") case object Alpha extends Alphabet
@CsvValue("B") case object Beta extends Alphabet
case object Gamma extends Alphabet

case class IntWrapper(value: Int)
case class IntResultWrapper(value: DecoderResult[Int])
case class Thing(value: String, extra: Int)
case class ThingWrapper(thing: Thing)
case class Wrapper[T](value: T)

class CellDecoderTest extends FlatSpec with Matchers with EitherValues {

  "derivation for coproducts" should "work out of the box for enum-style sealed traits" in {
    val simpleDecoder: CellDecoder[Simple] = semiauto.deriveCellDecoder

    simpleDecoder("On") shouldBe Right(On)
    simpleDecoder("Off") shouldBe Right(Off)
    simpleDecoder("foo").isLeft shouldBe true
  }

  it should "handle non-case object cases" in {
    implicit val numberedDecoder: CellDecoder[Numbered] = CellDecoder[Int].map(Numbered)
    implicit val unknownDecoder: CellDecoder[Unknown] = CellDecoder[String].map(Unknown)
    val complexDecoder: CellDecoder[Complex] = semiauto.deriveCellDecoder

    complexDecoder("Active") shouldBe Right(Active)
    complexDecoder("Inactive") shouldBe Right(Inactive)
    complexDecoder("inactive") shouldBe Right(Unknown("inactive"))
    complexDecoder("7") shouldBe Right(Numbered(7))
    complexDecoder("foo") shouldBe Right(Unknown("foo"))
  }

  it should "respect @CsvName annotations" in {
    val alphabetDecoder: CellDecoder[Alphabet] = semiauto.deriveCellDecoder

    Annotation[CsvValue, Alpha.type].apply().value shouldBe "A"

    alphabetDecoder("A") shouldBe Right(Alpha)
    alphabetDecoder("B") shouldBe Right(Beta)
    alphabetDecoder("Gamma") shouldBe Right(Gamma)
    alphabetDecoder("foobar").isLeft shouldBe true
  }

  "derivation for unary products" should "work for standard types" in {
    semiauto.deriveCellDecoder[IntResultWrapper].apply("7") shouldBe Right(IntResultWrapper(Right(7)))
  }

  it should "work for types with implicit decoder" in {
    implicit val thingDecoder: CellDecoder[Thing] = CellDecoder[String].map(Thing(_, 7))
    semiauto.deriveCellDecoder[ThingWrapper].apply("cell") shouldBe Right(ThingWrapper(Thing("cell", 7)))
  }

  it should "work for types with arguments" in {
    semiauto.deriveCellDecoder[Wrapper[Int]].apply("7") shouldBe Right(Wrapper(7))
  }
}
