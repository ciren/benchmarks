package benchmarks
package matrix

import org.scalacheck._
import org.scalacheck.Prop._

import cilib._

import shapeless._
import spire.math.{abs,Interval,sqrt}
import spire.implicits._
import matrix.implicits._
import dimension._
import dimension.implicits._
import benchmarks.Generators.arbInterval

object MatrixTests extends Properties("Matrix Tests") {

  property("Alpha matrix") = {
    val m = Matrix.alpha[nat._2,Double](1.0)
    val n = Matrix.alpha[nat._2,Double](2.0)
    val o = Matrix.wrap[nat._2,nat._2,Double](Vector(1.0, 0.0),Vector(0.0, sqrt(2)))
    val i = Matrix.eye[nat._2,Double]
    m === i &&
    n === o
  }

  property("Random matrix") = forAll { (i: Interval[Double]) =>
    val m = Matrix.random[nat._2](i) eval RNG.fromTime

    m.size === 2 &&
    m.forall(_.size === 2) &&
    m.toList.flatten.distinct.size == 4 &&
    m.forall(_.forall(mi => mi >= -100.0 && mi <= 100))
  }

  property("Rotation matrix") = forAll { (i: Interval[Double]) =>
    val m = Matrix.rotation[nat._2] eval RNG.fromTime
    val d = Dimension.random[nat._2,Double](i) eval RNG.fromTime
    val r = m * d

    abs(d.magnitude - r.magnitude) < 1E-10
  }

}
