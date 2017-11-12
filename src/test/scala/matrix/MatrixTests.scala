package benchmarks
package matrix

import org.scalacheck._
import org.scalacheck.Prop._

import shapeless._
import spire.math.sqrt
import spire.implicits._
import matrix.implicits._

object MatrixTests extends Properties("Matrix Tests") {

  property("Alpha matrix") = {
    val m = Matrix.alpha[nat._2,Double](1.0)
    val n = Matrix.alpha[nat._2,Double](2.0)
    val o = Matrix.wrap[nat._2,nat._2,Double](Vector(1.0, 0.0),Vector(0.0, sqrt(2)))
    val i = Matrix.eye[nat._2,Double]
    m === i &&
    n === o
  }

}
