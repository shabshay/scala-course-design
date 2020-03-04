package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal {
      val aVal = a()
      val bVal = b()
      val cVal = c()

      (bVal * bVal) - (4 * aVal * cVal)
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val aVal = a()
      val bVal = b()
      val cVal = c()
      val deltaVal = delta()

      val sol1 = ((-1 * bVal) + math.sqrt(deltaVal)) / (2 * aVal)
      val sol2 = ((-1 * bVal) - math.sqrt(deltaVal)) / (2 * aVal)

      Set(sol1, sol2)
    }
  }
}
