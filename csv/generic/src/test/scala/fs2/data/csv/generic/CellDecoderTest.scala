package fs2.data.csv
package generic

import cats.implicits._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.Annotation

class CellDecoderTest extends AnyFlatSpec with Matchers with EitherValues {

  "derivation for coproducts" should "work out of the box for enum-style sealed traits" in {
    val simpleDecoder: CellDecoder[Simple] = semiauto.deriveCellDecoder

    simpleDecoder("On") shouldBe Right(On)
    simpleDecoder("Off") shouldBe Right(Off)
    simpleDecoder("foo").isLeft shouldBe true
  }

  it should "handle non-case object cases" in {
    implicit val numberedDecoder: CellDecoder[Numbered] =
      CellDecoder[Int].map(Numbered)
    implicit val unknownDecoder: CellDecoder[Unknown] =
      CellDecoder[String].map(Unknown)
    val complexDecoder: CellDecoder[Complex] = semiauto.deriveCellDecoder

    complexDecoder("Active") shouldBe Right(Active)
    complexDecoder("Inactive") shouldBe Right(Inactive)
    complexDecoder("inactive") shouldBe Right(Unknown("inactive"))
    complexDecoder("7") shouldBe Right(Numbered(7))
    complexDecoder("foo") shouldBe Right(Unknown("foo"))
  }

  it should "respect @CsvValue annotations" in {
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
    implicit val thingDecoder: CellDecoder[Thing] =
      CellDecoder[String].map(Thing(_, 7))
    semiauto.deriveCellDecoder[ThingWrapper].apply("cell") shouldBe Right(ThingWrapper(Thing("cell", 7)))
  }

  it should "work for types with arguments" in {
    semiauto.deriveCellDecoder[Wrapper[Int]].apply("7") shouldBe Right(Wrapper(7))
  }
}
