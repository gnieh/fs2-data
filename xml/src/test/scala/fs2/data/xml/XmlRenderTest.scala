/*
 * Copyright 2024 fs2-data Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fs2.data.xml

import cats.effect.IO
import weaver._

object XmlRenderTest extends SimpleIOSuite {

  test("renders xml with self-closing tags") {
    xml"""<?xml version="1.0" encoding="utf-8"?><doc><no-content/></doc>"""
      .lift[IO]
      .through(render.raw())
      .compile
      .string
      .map { result =>
        expect.eql("""<?xml version="1.0" encoding="utf-8"?><doc><no-content/></doc>""", result)
      }
  }

  test("renders xml with self-closing tags prettily") {
    xml"""<?xml version="1.0" encoding="utf-8"?><doc><no-content/></doc>"""
      .lift[IO]
      .through(render.prettyPrint(width = 0))
      .compile
      .string
      .map { result =>
        expect.eql(
          """<?xml version="1.0"
          |      encoding="utf-8"?>
          |<doc>
          |  <no-content />
          |</doc>""".stripMargin,
          result
        )
      }
  }

  test("renders xml without self-closing tags if disabled") {
    xml"""<?xml version="1.0" encoding="utf-8"?><doc><no-content/></doc>"""
      .lift[IO]
      .through(render.raw(false))
      .compile
      .string
      .map { result =>
        expect.eql("""<?xml version="1.0" encoding="utf-8"?><doc><no-content></no-content></doc>""", result)
      }
  }

  test("renders xml with attributes prettily if it fits on one line") {
    xml"""<?xml version="1.0" encoding="utf-8"?><doc a1="value1" a2="value2"><no-content/></doc>"""
      .lift[IO]
      .through(render.prettyPrint(width = 40))
      .compile
      .string
      .map { result =>
        expect.eql(
          """<?xml version="1.0" encoding="utf-8"?>
          |<doc a1="value1" a2="value2">
          |  <no-content />
          |</doc>""".stripMargin,
          result
        )
      }
  }

  test("renders xml with attributes prettily if it doesn't fit on one line") {
    xml"""<?xml version="1.0" encoding="utf-8"?><doc a1="value1" a2="value2" a3="value3" a4="value4"><no-content/></doc>"""
      .lift[IO]
      .through(render.prettyPrint(width = 0))
      .compile
      .string
      .map { result =>
        expect.eql(
          """<?xml version="1.0"
          |      encoding="utf-8"?>
          |<doc a1="value1"
          |     a2="value2"
          |     a3="value3"
          |     a4="value4">
          |  <no-content />
          |</doc>""".stripMargin,
          result
        )
      }
  }

  test("renders text prettily") {
    xml"""<?xml version="1.0" encoding="utf-8"?><doc>This is a test.
The text is not originally formatted.</doc>"""
      .lift[IO]
      .through(render.prettyPrint(width = 20))
      .compile
      .string
      .map { result =>
        expect.eql(
          """<?xml version="1.0"
          |      encoding="utf-8"?>
          |<doc>
          |  This is a test. The text
          |  is not originally formatted.
          |</doc>""".stripMargin,
          result
        )
      }
  }

  test("renders text with entities prettily") {
    xml"""<?xml version="1.0" encoding="utf-8"?><doc>This is a test.
The text is not originally formatted but contains &amp; and
&acute; as entities.</doc>"""
      .lift[IO]
      .through(render.prettyPrint(width = 30))
      .compile
      .string
      .map { result =>
        expect.eql(
          """<?xml version="1.0"
          |      encoding="utf-8"?>
          |<doc>
          |  This is a test. The text is not
          |  originally formatted but contains
          |  &amp; and &acute; as entities.
          |</doc>""".stripMargin,
          result
        )
      }
  }

  test("renders CDATA as-is") {
    xml"""<?xml version="1.0" encoding="utf-8"?><doc><![CDATA[This is a test.
The text is not originally formatted.]]></doc>"""
      .lift[IO]
      .through(render.prettyPrint(width = 0))
      .compile
      .string
      .map { result =>
        expect.eql(
          """<?xml version="1.0"
          |      encoding="utf-8"?>
          |<doc>
          |  <![CDATA[This is a test.
          |The text is not originally formatted.]]>
          |</doc>""".stripMargin,
          result
        )
      }
  }

  test("renders comments prettily") {
    rawxml"""<?xml version="1.0" encoding="utf-8"?><doc><!-- This is a comment. --></doc>"""
      .lift[IO]
      .through(render.prettyPrint(width = 20))
      .compile
      .string
      .map { result =>
        expect.eql(
          """<?xml version="1.0"
          |      encoding="utf-8"?>
          |<doc>
          |  <!--
          |  This is a comment.
          |  -->
          |</doc>""".stripMargin,
          result
        )
      }
  }

}
