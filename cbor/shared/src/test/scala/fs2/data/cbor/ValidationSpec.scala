package fs2
package data
package cbor

import low._

import fs2._

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scodec.bits._

class ValidationSpec extends AnyFunSpec with Matchers {

  it("should fail if not enough array elements are provided") {
    Stream(CborItem.StartArray(4), CborItem.True, CborItem.Null)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail if not enough Map elements are provided") {
    Stream(CborItem.StartMap(2), CborItem.True, CborItem.Null)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail if an odd number of Map elements are provided") {
    Stream(CborItem.StartMap(2), CborItem.True, CborItem.Null, CborItem.False)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail if an odd number of indefinite map elements are provided") {
    Stream(CborItem.StartMap(2), CborItem.True, CborItem.Null, CborItem.Break)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail if unmatched break is given") {
    Stream(CborItem.True, CborItem.Break)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for unclosed indefinite array") {
    Stream(CborItem.StartIndefiniteArray, CborItem.True)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for unclosed indefinite map") {
    Stream(CborItem.StartIndefiniteMap, CborItem.True, CborItem.Null)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for unclosed indefinite text string") {
    Stream(CborItem.StartIndefiniteTextString)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for nested indefinite text string") {
    val res =
    Stream(CborItem.StartIndefiniteTextString,
           CborItem.TextString(""),
           CborItem.StartIndefiniteTextString,
           CborItem.Break,
           CborItem.Break)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for invalid indefinite text string elements") {
    val res =
    Stream(CborItem.StartIndefiniteTextString,
           CborItem.TextString(""),
           CborItem.True,
           CborItem.Break)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }


  it("should fail for unclosed indefinite byte string") {
    Stream(CborItem.StartIndefiniteByteString, CborItem.ByteString(hex""), CborItem.ByteString(hex"12"))
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for nested indefinite byte string") {
    val res =
    Stream(CborItem.StartIndefiniteByteString,
           CborItem.ByteString(hex""),
           CborItem.StartIndefiniteByteString,
           CborItem.Break,
           CborItem.Break)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for invalid indefinite byte string elements") {
    val res =
    Stream(CborItem.StartIndefiniteByteString,
           CborItem.ByteString(hex""),
           CborItem.True,
           CborItem.Break)
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for invalid number of positive integer bits") {
    Stream(CborItem.PositiveInt(hex"010101"))
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for invalid number of negative integer bits") {
    Stream(CborItem.NegativeInt(hex"010101"))
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for invalid number of half float bits") {
    Stream(CborItem.Float16(hex"010132"))
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for invalid number of float bits") {
    Stream(CborItem.Float32(hex"010132"))
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

  it("should fail for invalid number of double bits") {
    Stream(CborItem.Float64(hex"010132"))
      .through(validate[Fallible])
      .compile
      .drain.isLeft shouldBe true
  }

}
