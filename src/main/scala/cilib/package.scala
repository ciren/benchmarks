package cilib

import scalaz.Monoid

package object benchmarks {

  implicit object DoubleMonoid extends Monoid[Double] {
    def zero = 0.0
    def append(a: Double, b: => Double) = a + b
  }

}
