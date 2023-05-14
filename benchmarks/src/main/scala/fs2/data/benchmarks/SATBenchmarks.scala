package fs2.data.sat

import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 5, time = 2)
@Measurement(iterations = 10, time = 2)
class SATBenchmarks {

  type Lit = Int
  type Formula = List[List[Lit]]

  implicit val cnf = new CNF[Formula, Lit] {

    override def not(lit: Lit): Lit = -lit

    override def make(formula: Formula): List[List[Lit]] = formula

  }

  val solver: Solver[Formula, Lit] = new Solver

  val R1 = 1
  val B1 = 2
  val G1 = 3
  val Y1 = 4
  val R2 = 5
  val B2 = 6
  val G2 = 7
  val Y2 = 8

  val problem = List(
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
  )

  @Benchmark
  def sat() = {
    solver.isSatisfiable(problem)
  }

}
