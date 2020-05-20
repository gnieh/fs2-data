package fs2.data.csv.generic

import cats.implicits._
import fs2.data.csv.CellEncoder
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import shapeless.Annotation

class CellEncoderTest extends AnyFlatSpec with Matchers with EitherValues {

  import models._

  "derivation for coproducts" should "work out of the box for enum-style sealed traits" in {
    val simpleEncoder: CellEncoder[Simple] = semiauto.deriveCellEncoder

    simpleEncoder(On) shouldBe "On"
    simpleEncoder(Off) shouldBe "Off"
  }

  it should "handle non-case object cases" in {
    implicit val numberedEncoder: CellEncoder[Numbered] =
      CellEncoder[Int].contramap(_.n)
    implicit val unknownEncoder: CellEncoder[Unknown] =
      CellEncoder[String].contramap(_.state)
    val complexEncoder: CellEncoder[Complex] = semiauto.deriveCellEncoder

    complexEncoder(Active) shouldBe "Active"
    complexEncoder(Inactive) shouldBe "Inactive"
    complexEncoder(Unknown("inactive")) shouldBe "inactive"
    complexEncoder(Numbered(7)) shouldBe "7"
    complexEncoder(Unknown("foo")) shouldBe "foo"
  }

  it should "respect @CsvValue annotations" in {
    val alphabetEncoder: CellEncoder[Alphabet] = semiauto.deriveCellEncoder

    Annotation[CsvValue, Alpha.type].apply().value shouldBe "A"

    alphabetEncoder(Alpha) shouldBe "A"
    alphabetEncoder(Beta) shouldBe "B"
    alphabetEncoder(Gamma) shouldBe "Gamma"
  }

  "derivation for unary products" should "work for standard types" in {
    semiauto.deriveCellEncoder[IntWrapper].apply(IntWrapper(7)) shouldBe "7"
  }

  it should "work for types with implicit encoder" in {
    implicit val thingEncoder: CellEncoder[Thing] =
      CellEncoder[String].contramap(_.value)
    semiauto
      .deriveCellEncoder[ThingWrapper]
      .apply(ThingWrapper(Thing("cell", 7))) shouldBe "cell"
  }

  it should "work for types with arguments" in {
    semiauto.deriveCellEncoder[Wrapper[Int]].apply(Wrapper(7)) shouldBe Right(
      )
  }
}
