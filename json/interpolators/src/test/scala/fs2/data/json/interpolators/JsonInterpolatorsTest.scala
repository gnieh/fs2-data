package fs2.data.json.interpolators

import fs2.data.json.SelectorParser
import weaver._

object JsonInterpolatorsTest extends SimpleIOSuite {

  pureTest("handles a selector literal correctly") {
    expect(Right(selector".[].b.c") == new SelectorParser[Either[Throwable, *]](".[].b.c").parse())
  }

}
