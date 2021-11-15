// package benchmarks
// package matrix
// package syntax

// import org.scalacheck._
// import org.scalacheck.Prop._

// import cilib._

// import benchmarks.implicits._
// import Generators._

// object MatrixSyntaxTests extends Properties("Matrix Syntax Tests") {

//   property("Scalar-matrix multiplication") = forAll { (i: Interval[Double], s: Double) =>
//     val m = Matrix.random[nat._2](i) eval RNG.fromTime
//     val r = m * s
//     r.size === 2 &&
//     r.forall(_.size === 2) &&
//     m.toList.flatten.zip(r.toList.flatten).forall { case (mi, ri) => ri === mi * s }
//   }

//   property("Scalar-matrix division") = forAll { (i: Interval[Double], s: Double) =>
//     val m = Matrix.random[nat._2](i) eval RNG.fromTime
//     val r = m / s
//     r.size === 2 &&
//     r.forall(_.size === 2) &&
//     m.toList.flatten.zip(r.toList.flatten).forall { case (mi, ri) => ri === mi / s }
//   }

//   property("Matrix-matrix multiplication") = {
//     val a = Matrix.wrap[nat._2,nat._2,Double](Vector(1.0, 2.0), Vector(3.0, 4.0))
//     val b = Matrix.wrap[nat._2,nat._2,Double](Vector(5.0, 6.0), Vector(7.0, 8.0))
//     val c = Matrix.wrap[nat._2,nat._2,Double](Vector(23.0, 34.0), Vector(31.0, 46.0))
//     val d = Matrix.wrap[nat._2,nat._2,Double](Vector(301.0, 446.0), Vector(409.0, 606.0))
//     val i = Matrix.eye[nat._2,Double]
//     val j = Matrix.eye[nat._10,Double]

//     (a |*| b) === c &&
//     (c |*| b) === d &&
//     (a |*| i) === a &&
//     (b |*| i) === b &&
//     (c |*| i) === c &&
//     (i |*| i) === i &&
//     (j |*| j) === j
//   }

// }
