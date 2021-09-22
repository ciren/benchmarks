// package benchmarks
// package dimension
// package syntax

// import org.scalacheck._
// import org.scalacheck.Prop._

// import shapeless._

// import cilib._

// import benchmarks.dimension._
// import benchmarks.dimension.implicits._
// import benchmarks.dimension.Generators._
// import benchmarks.matrix.Matrix

// object DimensionSyntaxTests extends Properties("Dimension Syntax Tests") {

//   val dimensionGen = gen50(-10, 10)

//   property("size") = forAll(dimensionGen) { d =>
//     d.size === d.unsized.length
//   }

//   property("zip with index") = forAll(dimensionGen) { d =>
//     def sumOfSeries(a: Int, b: Int, n: Int) = n * (a + b) / 2

//     d.zipWithIndex.map { case (di, i) => i }.sum === sumOfSeries(0, d.size - 1, d.size)
//   }

//   property("zip") = forAll(dimensionGen) { d =>
//     (d zip d).size === d.size
//     ((d zip d) map { case (di, ei) => di * ei }) === d.map(_ ** 2)
//   }

//   property("mapSum") = forAll(dimensionGen) { d =>
//     d.mapSum(di => di) === d.sum &&
//     d.mapSum(_ ** 2)   === d.map(_ ** 2).sum
//   }

//   property("mapProduct") = forAll(dimensionGen) { d =>
//     d.mapProduct(di => di) === d.product &&
//     d.mapProduct(_ ** 2)   === d.map(_ ** 2).product
//   }

//   property("forAll") = forAll(dimensionGen) { d =>
//     val p = (xi: Int) => xi >= -10 && xi <= 10
//     d.forAll(p) &&
//     !(d forAll(_ > 10))
//   }

//   property("-") = forAll(dimensionGen) { d =>
//     (d - d) === d.map(_ => 0) &&
//     (d - d).sum === 0
//   }

//   property("+") = forAll(dimensionGen) { d =>
//     (d + d) === d.map(_ * 2) &&
//     (d + d).sum === (d.sum + d.sum)
//   }

//   property("innerProduct") = forAll(dimensionGen) { d =>
//     (d innerProduct d) === d.mapSum(xi => xi * xi)
//   } && {
//     val a = Sized(1.0, 2.0)
//     val b = Sized(3.0, 4.0)
//     val c = Sized(5.0, 6.0)

//     (a innerProduct b) === (b innerProduct a) &&
//     (a innerProduct (b + c)) === ((a innerProduct b) + (a innerProduct c))
//   }

//   property("randomize") = forAll(dimensionGen) { d =>
//     d.map(_.toDouble).randomize(Interval(0.0, 1.0)).eval(RNG.fromTime).size === d.size
//   }

//   property("rotate") = forAll(dimensionGen) { d =>
//     (d rotate Matrix.eye[_50,Int]) === d
//   } && {
//     val a = Sized(1.0, 2.0)
//     val m = Matrix.wrap[nat._2,nat._2,Double](
//       Vector(1.0, 2.0),
//       Vector(3.0, 4.0)
//     )

//     (a rotate m).unsized.toVector === Sized(7.0, 10.0).unsized.toVector
//   }

//   property("transpose") = forAll(dimensionGen) { d =>
//     d.t.size === d.size &&
//     d.t.forAll(_.size === 1)
//   }

//   property("magnitude") = forAll(dimensionGen) { d =>
//     d.map(_.toDouble).magnitude >= 0.0
//   } && {
//     Sized(0.0, 0.0, 0.0).magnitude === 0.0 &&
//     Sized(1.0, 2.0).magnitude === 5.0.sqrt &&
//     Sized(1.0, 2.0, 3.0).magnitude === 14.0.sqrt
//   }

// }
