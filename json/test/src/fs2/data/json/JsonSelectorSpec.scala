package fs2.data
package json

import ast._
import selector._

import fs2._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import cats.effect.SyncIO

abstract class JsonSelectorSpec[Json](implicit builder: Builder[Json], tokenizer: Tokenizer[Json])
    extends AnyFlatSpec
    with Matchers
    with Inside {

  "mandatory fields" should "fail in the missing single case" in {
    val selector = root.field("field").!.compile
    val filtered = Stream(Token.StartObject, Token.Key("other-field"), Token.TrueValue, Token.EndObject)
      .through(filter[Fallible](selector))
      .compile
      .drain

    inside(filtered) { case Left(e: JsonMissingFieldException) =>
      e.missing shouldBe Set("field")
    }
  }

  it should "fail in case at least one is missing" in {
    val selector = root.fields("field1", "field2", "field3").!.compile
    val filtered = Stream(Token.StartObject, Token.Key("field2"), Token.TrueValue, Token.EndObject)
      .through(filter[Fallible](selector))
      .compile
      .drain

    inside(filtered) { case Left(e: JsonMissingFieldException) =>
      e.missing shouldBe Set("field1", "field3")
    }
  }

  it should "fail in missing nested cases" in {
    val selector = root.iterate.field("field").!.compile
    val filtered = Stream(Token.StartArray,
                          Token.StartObject,
                          Token.Key("other-field"),
                          Token.TrueValue,
                          Token.EndObject,
                          Token.EndArray)
      .through(filter[Fallible](selector))
      .compile
      .drain

    inside(filtered) { case Left(e: JsonMissingFieldException) =>
      e.missing shouldBe Set("field")
    }
  }

  it should "fail on outermost error in case of nested missing keys" in {
    val selector = root.field("field1").!.field("field2").!.compile
    val filtered =
      Stream(Token.StartObject, Token.Key("other-field"), Token.StartObject, Token.EndObject, Token.EndObject)
        .through(filter[Fallible](selector))
        .compile
        .drain

    inside(filtered) { case Left(e: JsonMissingFieldException) =>
      e.missing shouldBe Set("field1")
    }
  }

  it should "success if all mandatory fields are present" in {
    val selector = root.fields("field1", "field2", "field3").!.compile
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

  "transformOpt" should "only transform the select element" in {
    val selector = root.field("f").compile
    val transformed =
      Stream(Token.StartObject,
             Token.Key("f"),
             Token.TrueValue,
             Token.Key("g"),
             Token.StringValue("test"),
             Token.EndObject)
        .through(transformOpt[Fallible, Json](selector, _ => Some(builder.makeFalse)))
        .compile
        .toList
    transformed shouldBe Right(
      List(Token.StartObject,
           Token.Key("f"),
           Token.FalseValue,
           Token.Key("g"),
           Token.StringValue("test"),
           Token.EndObject))
  }

  it should "filter out the value if it returns None" in {
    val selector = root.field("f").compile
    val transformed =
      Stream(Token.StartObject,
             Token.Key("f"),
             Token.TrueValue,
             Token.Key("g"),
             Token.StringValue("test"),
             Token.EndObject)
        .through(transformOpt[Fallible, Json](selector, _ => None))
        .compile
        .toList
    transformed shouldBe Right(List(Token.StartObject, Token.Key("g"), Token.StringValue("test"), Token.EndObject))
  }

  it should "filter out the innermost value if it returns None" in {
    val selector = root.field("f").field("g").compile
    val transformed =
      Stream(
        Token.StartObject,
        Token.Key("f"),
        Token.StartObject,
        Token.Key("g"),
        Token.TrueValue,
        Token.EndObject,
        Token.Key("g"),
        Token.StringValue("test"),
        Token.EndObject
      ).through(transformOpt[Fallible, Json](selector, _ => None)).compile.toList
    transformed shouldBe Right(
      List(Token.StartObject,
           Token.Key("f"),
           Token.StartObject,
           Token.EndObject,
           Token.Key("g"),
           Token.StringValue("test"),
           Token.EndObject))
  }

  "transformF" should "fail the stream if one value fails" in {
    val selector = root.field("f").compile
    val exn = new Exception
    val transformed =
      Stream(Token.StartObject,
             Token.Key("f"),
             Token.TrueValue,
             Token.Key("g"),
             Token.StringValue("test"),
             Token.EndObject)
        .through(transformF[SyncIO, Json](selector, _ => SyncIO.raiseError(exn)))
        .attempt
        .compile
        .toList
        .unsafeRunSync()
    transformed shouldBe List(Right(Token.StartObject), Right(Token.Key("f")), Left(exn))
  }

}
