package benchmarks
package syntax

import scala.language.implicitConversions

import spire.algebra.{ Ring }
import spire.implicits._

final class RangeOps(x: Range) {
  def mapSum[A](f: Int => A)(implicit ev: Ring[A]): A =
    x.toList.foldLeft(ev.zero)((b: A, a: Int) => b + f(a))
}

trait RangeSyntax {
  implicit def ToRangeOps(v: Range): RangeOps = new RangeOps(v)
}
