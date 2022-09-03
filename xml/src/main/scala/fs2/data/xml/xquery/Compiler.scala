package fs2.data
package xml
package xquery

import cats.Defer
import cats.MonadError
import cats.syntax.all._

import scala.annotation.tailrec

import mft._
import xpath.{Axis, Location, Predicate}

class Compiler[F[_]](implicit F: MonadError[F, Throwable], defer: Defer[F]) {

  /** Compiles an XQuery expression, that can be reused over several streams. */
  def compile(query: Query): F[CompiledQuery[F]] = {

    val mft = dsl[Predicate, XmlInput, XmlOutput] { implicit builder =>
      val init = state(args = 0, initial = true)
      val init1 = state(args = 1)
      val cop = state(args = 0)

      init(any) -> init1(x0, cop(x0))

      cop(anyNode) -> copy(cop(x1)) ~ cop(x2)
      cop(anyLeaf) -> copy ~ cop(x1)
      cop(epsilon) -> eps

      def compileXPathTrunk(path: List[Location], orig: builder.StateBuilder, dest: builder.StateBuilder) = {
        val qfinal = path.size - 1
        path.zipWithIndex.foldLeft((orig, () => ())) {
          case ((src, dflt), (Location(Axis.Child, n, pred), idx)) if idx < qfinal =>
            val tgt = state(args = orig.nargs)

            val matching = node(XmlInput(n, pred))
            val copyArgs = List.range(0, src.nargs).map(y(_))

            src(matching) -> tgt(x1, copyArgs) ~ src(x2, copyArgs)
            src(anyNode) -> src(x1, copyArgs) ~ src(x2, copyArgs)

            tgt(matching) -> tgt(x1, copyArgs) ~ tgt(x2, copyArgs)

            dflt()

            (tgt, () => tgt(anyNode) -> src(x1, copyArgs) ~ src(x2, copyArgs))
        }
      }

      def translate(tree: Tree, context: Map[String, Int], q: builder.StateBuilder): Unit =
        tree match {
          case ListClause(es) =>
            val forwardParams = List.range(0, q.nargs).map(Rhs.Param(_))
            q(any) ->
              es.foldLeft[Rhs[XmlOutput]](eps) { (acc, child) =>
                val q1 = state(args = q.nargs)
                translate(child, context, q1)
                acc ~ q1(x0, forwardParams)
              }
          case Element(name, children) =>
            val q1 = state(args = q.nargs)

            val forwardParams = List.range(0, q.nargs).map(Rhs.Param(_))

            q(any) -> node(XmlOutput.Element(name), q1(x0, forwardParams))

            q1(any) ->
              children.foldLeft[Rhs[XmlOutput]](eps) { (acc, child) =>
                val q1 = state(args = q.nargs)
                translate(child, context, q1)
                acc ~ q1(x0, forwardParams)
              }
          case Str(s) =>
            q(any) -> leaf(XmlOutput.Text(s))
          case Ordpath(v, None) =>
            context.get(v).foreach { n =>
              q(any) -> y(n)
            }
          case Ordpath(v, Some(p)) =>
            compileXPathTrunk(p, ???, ???)
            ???
          case ForClause(v, src, e) =>
            val q1 = state(args = q.nargs + 1)
            ???
          case LetClause(v, src, e) =>
            ???
        }

    }

    mft.esp[F].map(new CompiledQuery(_))
  }

  /** Compiles an XPath to a DFA.
    * This function takes advantage of the structure of our XPath
    * version, that has for each step either a child or a descendant (star) axis.
    * Basically at each step the NFA has at most 2 transitions:
    *  - step (child and descendant)
    *  - loop (descendant)
    * Moreover since a path is linear, NFA states can be numbered strictly increasingly
    * (0 is initial, and the only final state is the highest int)
    *
    * All these assumpations make it possible to have shortcut in the determinization.
    */
  def makeDFA(path: List[Location]) = {
    val nfa =
      path.zipWithIndex.map {
        case (Location(Axis.Child, n, p), src) =>
          (n, p, src + 1, false)
        case (Location(Axis.Descendant, n, p), src) =>
          (n, p, src + 1, true)
      }.toVector

    val _ = nfa.zipWithIndex.map {
      case ((_, _, tgt, true), src) => List(tgt, src)
      case ((_, _, tgt, false), _)  => List(tgt)
    }
    ???
  }

}
