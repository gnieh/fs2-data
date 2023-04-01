package fs2
package data
package cbor

import cats.Show
import fs2.data.cbor.high.CborValue
import org.scalacheck.Arbitrary
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object RoundtripTest extends SimpleIOSuite with Checkers {

  implicit val show: Show[CborValue] = Show.show(_.toString)

  test("roundtrips numbers") {
    forall(Arbitrary.arbitrary[BigInt].map(CborValue.Integer(_): CborValue)) { value =>
      Stream(value)
        .through(high.toBinary)
        .through(high.values)
        .compile
        .onlyOrError
        .map(r => expect.same(value, r))
    }
  }

}
