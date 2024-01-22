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
