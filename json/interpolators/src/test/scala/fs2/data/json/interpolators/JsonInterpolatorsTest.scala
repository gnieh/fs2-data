package fs2.data.json.interpolators

import fs2.data.json.SelectorParser
import weaver._

object JsonInterpolatorsTest extends SimpleIOSuite {

  pureTest("handles a selector literal correctly") {
    val sel = selector".[].b.c"
    val manual = new SelectorParser[Either[Throwable, *]](".[].b.c").parse()
    expect.same(manual, Right(sel))
  }

}
