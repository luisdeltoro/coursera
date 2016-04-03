package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal[Set[Double]] {
      if (delta() < 0) Set()
      else computeRoots(a(), b(), delta())
    }
  }

  private def computeRoots(a: Double, b: Double, delta: Double): Set[Double] = {
    val x1 = (-b + math.sqrt(delta)) / (2 * a)
    val x2 = (-b - math.sqrt(delta)) / (2 * a)
    if (x1 == x2) Set(x1)
    else Set(x1, x2)
  }
}
