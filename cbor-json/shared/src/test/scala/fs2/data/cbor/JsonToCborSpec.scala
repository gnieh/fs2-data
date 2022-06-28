package fs2
package data
package json

import fs2.data.cbor.low.CborItem

import scodec.bits._

import weaver._
import fs2.data.cbor.Tags

object JsonToCborSpec extends SimpleIOSuite {

  pureTest("valid Long should be encoded into CBOR integer") {
    val items =
      Stream
        .emits(
          List(
            Token.NumberValue("0"),
            Token.NumberValue("10"),
            Token.NumberValue("-1"),
            Token.NumberValue("-20"),
            Token.NumberValue(Long.MinValue.toString()),
            Token.NumberValue(Long.MaxValue.toString())
          ))
        .through(cbor.encodeItems)
        .compile
        .toList
    expect(
      items == List(
        CborItem.PositiveInt(hex"00"),
        CborItem.PositiveInt(hex"0a"),
        CborItem.NegativeInt(hex"00"),
        CborItem.NegativeInt(hex"13"),
        CborItem.NegativeInt(hex"7fffffffffffffff"),
        CborItem.PositiveInt(hex"7fffffffffffffff")
      ))
  }

  pureTest("integers not fitting in Long are encoded as big numbers") {
    val items =
      Stream
        .emits(
          List(
            Token.NumberValue((BigInt(Long.MinValue) - 1).toString()),
            Token.NumberValue((BigInt(Long.MaxValue) + 1).toString())
          ))
        .through(cbor.encodeItems)
        .compile
        .toList
    expect(
      items == List(
        CborItem.Tag(Tags.NegativeBigNum),
        CborItem.ByteString(hex"8000000000000000"),
        CborItem.Tag(Tags.PositiveBigNum),
        CborItem.ByteString(hex"8000000000000000")
      ))
  }

  pureTest("valid floats are encoded as float32") {
    val items =
      Stream
        .emits(
          List(
            Token.NumberValue("1.25"),
            Token.NumberValue("1e500")
          ))
        .through(cbor.encodeItems)
        .compile
        .toList
    expect(
      items == List(
        CborItem.Float32(hex"3fa00000"),
        CborItem.Tag(Tags.PositiveBigNum),
        CborItem.ByteString(
          hex"1f365fec9370d2baee34b150720878ea52ccba5e661e2f4fbda27e2b43bc65e952814f46187bf47646f960e57615b1b14565571de604d10b58df55762cd1b1d82d412b739baf43048fbf621de5493a15049a5ce8703d84990f0bcf9a1b88e0179c93273fff1641f92f53e9ccc53f7883cb08bdee813899aed8283c15eebd4372dad168bab87080f7d40d71a207f3ab7b6b100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
      ))
  }

}
