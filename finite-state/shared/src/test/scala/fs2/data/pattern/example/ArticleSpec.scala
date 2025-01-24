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

package fs2
package data
package pattern
package example

import esp.{ESP, Conversion}
import mft._

import weaver._

import cats.effect._

object ArticleSpec extends IOSuite {

  type Res = ESP[IO, NoGuard, String, String]

  implicit object StringConversions extends Conversion[String, MiniXML] {

    override def makeOpen(t: String): MiniXML = MiniXML.Open(t)

    override def makeClose(t: String): MiniXML = MiniXML.Close(t)

    override def makeLeaf(t: String): MiniXML = MiniXML.Text(t)

  }

  override def sharedResource: Resource[IO, Res] =
    Resource.eval {

      val mft: MFT[NoGuard, String, String] = dsl { implicit builder =>
        val Main = state(args = 0, initial = true)
        val Title = state(args = 0)
        val InArticle = state(args = 1)
        val Key2Em = state(args = 0)
        val AllKeys = state(args = 0)
        val Copy = state(args = 0)

        Main(aNode("article")) ->
          node("html") {
            node("head") {
              Title(x1)
            } ~ node("body") {
              InArticle(x1, eps)
            }
          }

        Title(aNode("title")) ->
          node("title") {
            Copy(x1)
          }

        InArticle(aNode("title")) ->
          node("h1") {
            Copy(x1)
          } ~ InArticle(x2, y(0))
        InArticle(aNode("para")) ->
          node("p") {
            Key2Em(x1)
          } ~ InArticle(x2, y(0) ~ AllKeys(x1))
        InArticle(aNode("postscript")) ->
          node("h2") {
            leaf("Index")
          } ~ node("ul") {
            y(0)
          } ~ node("h2") {
            leaf("Postscript")
          } ~ node("p") {
            Copy(x1)
          }

        Key2Em(aNode("key")) ->
          node("em") {
            Copy(x1)
          } ~ Key2Em(x2)
        Key2Em(anyLeaf) ->
          copy ~ Key2Em(x1)
        Key2Em(epsilon) ->
          eps

        AllKeys(aNode("key")) ->
          node("li") {
            Copy(x1)
          } ~ AllKeys(x2)
        AllKeys(anyLeaf) ->
          AllKeys(x1)
        AllKeys(epsilon) ->
          eps

        Copy(anyNode) ->
          copy(Copy(x1)) ~ Copy(x2)
        Copy(anyLeaf) ->
          copy ~ Copy(x1)
        Copy(epsilon) ->
          eps
      }

      mft.esp
    }

  test("article to html") { esp =>
    Stream
      .emits(List[MiniXML](
          // format: off
          MiniXML.Open("article"),
            MiniXML.Open("title"),
              MiniXML.Text("MFT"),
            MiniXML.Close("title"),
            MiniXML.Open("para"),
              MiniXML.Text(" XML is "),
              MiniXML.Open("key"),
                MiniXML.Text("forest"),
              MiniXML.Close("key"),
              MiniXML.Text(". "),
            MiniXML.Close("para"),
            MiniXML.Open("para"),
              MiniXML.Text(" "),
              MiniXML.Open("key"),
                MiniXML.Text("MFT"),
              MiniXML.Close("key"),
              MiniXML.Text(" transforms forests. "),
            MiniXML.Close("para"),
            MiniXML.Open("para"),
              MiniXML.Text(" MFT transforms XML. "),
            MiniXML.Close("para"),
            MiniXML.Open("postscript"),
              MiniXML.Text(" MFT is quite expressive. "),
            MiniXML.Close("postscript"),
          MiniXML.Close("article"),
          // format: on
        ))
      .covary[IO]
      .through(esp.pipe[MiniXML, MiniXML])
      .compile
      .toList
      .map { events =>
        expect.eql(
          List[MiniXML](
            // format: off
            MiniXML.Open("html"),
              MiniXML.Open("head"),
                MiniXML.Open("title"),
                  MiniXML.Text("MFT"),
                MiniXML.Close("title"),
              MiniXML.Close("head"),
              MiniXML.Open("body"),
                MiniXML.Open("h1"),
                  MiniXML.Text("MFT"),
                MiniXML.Close("h1"),
                MiniXML.Open("p"),
                  MiniXML.Text(" XML is "),
                  MiniXML.Open("em"),
                    MiniXML.Text("forest"),
                  MiniXML.Close("em"),
                  MiniXML.Text(". "),
                MiniXML.Close("p"),
                MiniXML.Open("p"),
                  MiniXML.Text(" "),
                  MiniXML.Open("em"),
                    MiniXML.Text("MFT"),
                  MiniXML.Close("em"),
                  MiniXML.Text(" transforms forests. "),
                MiniXML.Close("p"),
                MiniXML.Open("p"),
                  MiniXML.Text(" MFT transforms XML. "),
                MiniXML.Close("p"),
                MiniXML.Open("h2"),
                  MiniXML.Text("Index"),
                MiniXML.Close("h2"),
                MiniXML.Open("ul"),
                  MiniXML.Open("li"),
                    MiniXML.Text("forest"),
                  MiniXML.Close("li"),
                  MiniXML.Open("li"),
                    MiniXML.Text("MFT"),
                  MiniXML.Close("li"),
                MiniXML.Close("ul"),
                MiniXML.Open("h2"),
                  MiniXML.Text("Postscript"),
                MiniXML.Close("h2"),
                MiniXML.Open("p"),
                  MiniXML.Text(" MFT is quite expressive. "),
                MiniXML.Close("p"),
              MiniXML.Close("body"),
            MiniXML.Close("html"),
            // format: on
          ),
          events
        )
      }
  }

}
