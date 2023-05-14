package fs2.data.sat

import weaver._

object SolverSpec extends SimpleIOSuite {

  type Lit = Int
  type Formula = List[List[Lit]]

  val a = 1
  val b = 2
  val c = 3

  implicit val cnf = new CNF[Formula, Lit] {

    override def not(lit: Lit): Lit = -lit

    override def make(formula: Formula): List[List[Lit]] = formula

  }

  val solver: Solver[Formula, Lit] = new Solver

  pureTest("simple sat") {
    expect(solver.isSatisfiable(List(List(a))))
  }

  pureTest("simple non-sat") {
    expect(!solver.isSatisfiable(List(List(a), List(-a))))
  }

  pureTest("sat") {
    expect(solver.isSatisfiable(List(List(a, b, -c), List(b, c), List(-b), List(-a, c))))
  }

  pureTest("4-colours sat") {
    val R1 = 1
    val B1 = 2
    val G1 = 3
    val Y1 = 4
    val R2 = 5
    val B2 = 6
    val G2 = 7
    val Y2 = 8
    expect(
      solver.isSatisfiable(List(
        List(R1, B1, G1, Y1),
        List(-R1, -B1),
        List(-R1, -G1),
        List(-R1, -Y1),
        List(-B1, -G1),
        List(-B1, -Y1),
        List(-G1, -Y1),
        List(R2, B2, G2, Y2),
        List(-R2, -B2),
        List(-R2, -G2),
        List(-R2, -Y2),
        List(-B2, -G2),
        List(-B2, -Y2),
        List(-G2, -Y2),
        List(-R1, -R2),
        List(-B1, -B2),
        List(-G1, -G2),
        List(-Y1, -Y2)
      )))
  }

}
