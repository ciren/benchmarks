package benchmarks
package dimension

import org.scalacheck._
import org.scalacheck.Prop.forAll

import shapeless._
import spire.implicits._

object DimensionTests extends Properties("Matrix Tests") {

  property("Dimension fill") = forAll { (x: Double) =>
    val a = Dimension.fill[nat._2,Double](x)
    val b = Dimension.fill[nat._22,Double](x)
    a.size === 2 &&
    b.size === 22 &&
    a.forall(_ == x) &&
    b.forall(_ == x)
  }

}
