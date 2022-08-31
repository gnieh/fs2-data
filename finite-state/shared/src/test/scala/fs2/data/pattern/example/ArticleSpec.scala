package fs2
package data
package pattern
package example

import esp.{ESP, Conversion}
import mft._

import weaver._

import cats.effect._

object ArticleSpec extends IOSuite {

  type Res = ESP[IO, String, String]

  val Main = 0
  val Title = 1
  val InArticle = 2
  val Key2Em = 3
  val AllKeys = 4
  val Copy = 5

  implicit object StringConversions extends Conversion[String, MiniXML] {

    override def makeOpen(t: String): MiniXML = MiniXML.Open(t)

    override def makeClose(t: String): MiniXML = MiniXML.Close(t)

    override def makeLeaf(t: String): MiniXML = MiniXML.Text(t)

  }

  override def sharedResource: Resource[IO, Res] =
    Resource.eval {

      val mft = new MFT[String, String](
        Main,
        Map(
          Main -> Rules(
            Nil,
            List(
              EventSelector.Node("article") -> Rhs.Node(
                "html",
                Rhs.Concat(Rhs.Node("head", Rhs.Call(Title, Forest.First, Nil)),
                           Rhs.Node("body", Rhs.Call(InArticle, Forest.First, List(Rhs.Epsilon))))))
          ),
          Title -> Rules(Nil,
                         List(EventSelector.Node("title") -> Rhs.Node("title", Rhs.Call(Copy, Forest.First, Nil)))),
          InArticle -> Rules(
            List(0),
            List(
              EventSelector.Node("title") -> Rhs.Concat(Rhs.Node("h1", Rhs.Call(Copy, Forest.First, Nil)),
                                                        Rhs.Call(InArticle, Forest.Second, List(Rhs.Param(0)))),
              EventSelector.Node("para") -> Rhs.Concat(
                Rhs.Node("p", Rhs.Call(Key2Em, Forest.First, Nil)),
                Rhs.Call(InArticle,
                         Forest.Second,
                         List(Rhs.Concat(Rhs.Param(0), Rhs.Call(AllKeys, Forest.First, Nil))))
              ),
              EventSelector.Node("postscript") -> Rhs.Concat(
                Rhs.Node("h2", Rhs.Leaf("Index")),
                Rhs.Concat(Rhs.Node("ul", Rhs.Param(0)),
                           Rhs.Concat(Rhs.Node("h2", Rhs.Leaf("Postscript")),
                                      Rhs.Node("p", Rhs.Call(Copy, Forest.First, Nil))))
              )
            )
          ),
          Key2Em -> Rules(
            Nil,
            List(
              EventSelector.Node("key") -> Rhs.Concat(Rhs.Node("em", Rhs.Call(Copy, Forest.First, Nil)),
                                                      Rhs.Call(Key2Em, Forest.Second, Nil)),
              EventSelector.AnyLeaf -> Rhs.Concat(Rhs.CopyLeaf, Rhs.Call(Key2Em, Forest.First, Nil)),
              EventSelector.Epsilon -> Rhs.Epsilon
            )
          ),
          AllKeys -> Rules(
            Nil,
            List(
              EventSelector.Node("key") -> Rhs.Concat(Rhs.Node("li", Rhs.Call(Copy, Forest.First, Nil)),
                                                      Rhs.Call(AllKeys, Forest.Second, Nil)),
              EventSelector.AnyLeaf -> Rhs.Call(AllKeys, Forest.First, Nil),
              EventSelector.Epsilon -> Rhs.Epsilon
            )
          ),
          Copy -> Rules(
            Nil,
            List(
              EventSelector.AnyNode -> Rhs.Concat(Rhs.CopyNode(Rhs.Call(Copy, Forest.First, Nil)),
                                                  Rhs.Call(Copy, Forest.Second, Nil)),
              EventSelector.AnyLeaf -> Rhs.Concat(Rhs.CopyLeaf, Rhs.Call(Copy, Forest.First, Nil)),
              EventSelector.Epsilon -> Rhs.Epsilon
            )
          )
        )
      )

      mft.esp
    }

  test("article to html") { esp =>
    Stream
      .emits(
        List[MiniXML](
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
