package fs2.data.cbor.high

import cats.effect.IO
import fs2._
import weaver.SimpleIOSuite

object ValueParserTest extends SimpleIOSuite {

  test("roundtrips bignums") {
    val in = CborValue.Integer(BigInt("-739421513997118914047232662242593364"))
    Stream(in).covary[IO].through(toBinary).through(values).compile.onlyOrError.map { out =>
      expect.same(in, out)
    }
  }

}
