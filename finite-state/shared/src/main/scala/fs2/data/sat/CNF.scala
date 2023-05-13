package fs2.data.sat

trait CNF[Formula, Lit] extends Literal[Lit] {
  def make(formula: Formula): List[List[Lit]]
}
