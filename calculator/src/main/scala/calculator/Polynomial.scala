package calculator

import scala.collection.immutable.HashSet

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Signal[Double](b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    new Signal[Set[Double]](new HashSet[Double]() + ((-b() + Math.sqrt(delta()))/(2 * a())) + ((-b() - Math.sqrt(delta()))/(2 * a())))
  }
}
