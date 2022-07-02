package fs2.data.automaton

trait Pred[P, Elt] {

  def apply(p: P)(e: Elt): Boolean

  def and(p1: P, p2: P): P

  def or(p1: P, p2: P): P

  def not(p: P): P

}
