/*
 * Copyright 2021 Lucas Satabin
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

package fs2
package data
package xml
package internals

private sealed trait MarkupToken {
  def render: String
}

private object MarkupToken {

  case class StartToken(name: QName) extends MarkupToken {
    def render: String = s"<${name.render}"
  }

  case class EndToken(name: QName) extends MarkupToken {
    def render: String = s"</${name.render}>"
  }

  case class PIToken(name: String) extends MarkupToken {
    def render: String = s"<?$name"
  }

  case class DeclToken(name: String) extends MarkupToken {
    def render: String = s"<!$name"
  }

  case object CommentToken extends MarkupToken {
    def render: String = "<!-- ... -->"
  }

  case object CDataToken extends MarkupToken {
    def render: String = "<![CDATA["
  }

}
