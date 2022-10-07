package fs2.data.xml

import cats.effect.IO
import cats.syntax.all._
import weaver._

object XmlRenderTest extends SimpleIOSuite {

  test("renders xml with self-closing tags") {
    val result =
      xml"""<?xml version="1.0" encoding="utf-8"?><doc><no-content/></doc>""".through(render()).compile.string
    result.liftTo[IO].map { result =>
      expect.eql("""<?xml version="1.0" encoding="utf-8"?><doc><no-content/></doc>""", result)
    }
  }

  test("renders xml without self-closing tags if disabled") {
    val result =
      xml"""<?xml version="1.0" encoding="utf-8"?><doc><no-content/></doc>""".through(render(false)).compile.string
    result.liftTo[IO].map { result =>
      expect.eql("""<?xml version="1.0" encoding="utf-8"?><doc><no-content></no-content></doc>""", result)
    }
  }

}
