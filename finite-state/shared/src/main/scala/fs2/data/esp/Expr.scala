package fs2.data.esp

sealed trait Expr[+Out]
object Expr {
  case class Call[Out](q: Int, depth: Int, params: List[Expr[Out]]) extends Expr[Out]
  case object Epsilon extends Expr[Nothing]
  case class Open[Out](open: Out, next: Expr[Out]) extends Expr[Out]
  case class Close[Out](close: Out, next: Expr[Out]) extends Expr[Out]
  case class Leaf[Out](value: Out, next: Expr[Out]) extends Expr[Out]
  case class Concat[Out](fst: Expr[Out], snd: Expr[Out]) extends Expr[Out]

  def concat[Out](e1: Expr[Out], e2: Expr[Out]): Expr[Out] =
    (e1, e2) match {
      case (Epsilon, _)           => e2
      case (_, Epsilon)           => e1
      case (Open(o, Epsilon), _)  => Open(o, e2)
      case (Close(c, Epsilon), _) => Close(c, e2)
      case (Leaf(v, Epsilon), _)  => Leaf(v, e2)
      case (_, _)                 => Concat(e1, e2)
    }
}
