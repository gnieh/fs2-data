package fs2.data.sat

trait Literal[Lit] {
  def not(lit: Lit): Lit
}
