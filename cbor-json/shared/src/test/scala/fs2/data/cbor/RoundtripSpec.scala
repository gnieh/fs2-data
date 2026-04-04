package fs2
package data

import cats.effect.IO
import fs2.data.cbor.high.CborValue
import weaver.SimpleIOSuite

object RoundtripSpec extends SimpleIOSuite {

  test("CBOR items should round-trip through JSON") {
    val items = List(
      CborValue.Integer(123456), // 3 bytes
      CborValue.Integer(1235213352876L), // 5 bytes
      CborValue.TextString("Hello, world!"),
      CborValue.True,
      CborValue.False,
      CborValue.Null,
      CborValue.Float64(Math.PI)
    )

    Stream
      .emits(items)
      .through(cbor.high.toItems)
      .through(cbor.json.decodeItems[IO])
      .through(json.cbor.encodeItems)
      .through(cbor.high.parseValues)
      .compile
      .toList
      .map { result => expect.same(items, result) }
  }

}
