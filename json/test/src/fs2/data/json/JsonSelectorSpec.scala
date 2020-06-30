package fs2
package data
package json

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside

class JsonSelectorSpec extends AnyFlatSpec with Matchers with Inside {

  "mandatory fields" should "fail in the missing single case" in {
    val selector = Selector.NameSelector("field", true, true)
    val filtered = Stream(Token.StartObject, Token.Key("other-field"), Token.TrueValue, Token.EndObject)
      .through(filter[Fallible](selector))
      .compile
      .drain

    inside(filtered) {
      case Left(e: JsonMissingFieldException) => e.missing shouldBe Set("field")
    }
  }

  it should "fail in case at least one is missing" in {
    val selector = Selector.NameSelector(Set("field1", "field2", "field3"), true, true)
    val filtered = Stream(Token.StartObject, Token.Key("field2"), Token.TrueValue, Token.EndObject)
      .through(filter[Fallible](selector))
      .compile
      .drain

    inside(filtered) {
      case Left(e: JsonMissingFieldException) => e.missing shouldBe Set("field1", "field3")
    }
  }

  it should "fail in missing nested cases" in {
    val selector = Selector.PipeSelector(Selector.IteratorSelector(true), Selector.NameSelector("field", true, true))
    val filtered = Stream(Token.StartArray,
                          Token.StartObject,
                          Token.Key("other-field"),
                          Token.TrueValue,
                          Token.EndObject,
                          Token.EndArray)
      .through(filter[Fallible](selector))
      .compile
      .drain

    inside(filtered) {
      case Left(e: JsonMissingFieldException) => e.missing shouldBe Set("field")
    }
  }

  it should "fail on outermost error in case of nested missing keys" in {
    val selector =
      Selector.PipeSelector(Selector.NameSelector("field1", true, true), Selector.NameSelector("field2", true, true))
    val filtered =
      Stream(Token.StartObject, Token.Key("other-field"), Token.StartObject, Token.EndObject, Token.EndObject)
        .through(filter[Fallible](selector))
        .compile
        .drain

    inside(filtered) {
      case Left(e: JsonMissingFieldException) => e.missing shouldBe Set("field1")
    }
  }

  it should "success if all mandatory fields are present" in {
    val selector = Selector.NameSelector(Set("field1", "field2", "field3"), true, true)
    val filtered = Stream(
      Token.StartObject,
      Token.Key("field2"),
      Token.TrueValue,
      Token.Key("field1"),
      Token.StringValue("test"),
      Token.Key("other-field"),
      Token.NullValue,
      Token.Key("field3"),
      Token.NumberValue("1"),
      Token.EndObject
    ).through(filter[Fallible](selector)).compile.drain

    filtered shouldBe Right(())
  }

}
