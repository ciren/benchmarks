package benchmarks
package matrix
package syntax

import org.scalacheck._
import org.scalacheck.Prop._

import shapeless._
import spire.implicits._

import benchmarks.implicits._

object MatrixSyntaxTests extends Properties("Matrix Syntax Tests") {

  val doubleGen = Gen.choose(-1000.0, 1000.0)
  val gen = Gen.listOf(doubleGen)

  property("Matrix-matrix multiplication") = forAll { (a: List[Double]) =>
    val a = Matrix.wrap[nat._2,nat._2,Double](Vector(1.0, 2.0), Vector(3.0, 4.0))
    val b = Matrix.wrap[nat._2,nat._2,Double](Vector(5.0, 6.0), Vector(7.0, 8.0))
    val c = Matrix.wrap[nat._2,nat._2,Double](Vector(23.0, 34.0), Vector(31.0, 46.0))
    val d = Matrix.wrap[nat._2,nat._2,Double](Vector(301.0, 446.0), Vector(409.0, 606.0))
    val i = Matrix.eye[nat._2,Double]
    val j = Matrix.eye[nat._10,Double]

    (a |*| b) === c &&
    (c |*| b) === d &&
    (a |*| i) === a &&
    (b |*| i) === b &&
    (c |*| i) === c &&
    (i |*| i) === i &&
    (j |*| j) === j
  }
}
