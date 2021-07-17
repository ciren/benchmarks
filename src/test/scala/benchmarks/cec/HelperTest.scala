// package benchmarks
// package cec

// import org.scalacheck._
// import org.scalacheck.Prop._

// import shapeless._
// import spire.implicits._

// import benchmarks.dimension._
// import benchmarks.matrix._
// import benchmarks.implicits._

// object HelperTest extends Properties("CEC Helper") {

//   val fbiases = List(
//     -4.5000000e+002, -4.5000000e+002, -4.5000000e+002, -4.5000000e+002,
//     -3.1000000e+002, 3.9000000e+002, -1.8000000e+002, -1.4000000e+002,
//     -3.3000000e+002, -3.3000000e+002, 9.0000000e+001, -4.6000000e+002,
//     -1.3000000e+002, -3.0000000e+002, 1.2000000e+002, 1.2000000e+002,
//     1.2000000e+002, 1.0000000e+001, 1.0000000e+001, 1.0000000e+001,
//     3.6000000e+002, 3.6000000e+002, 3.6000000e+002, 2.6000000e+002,
//     2.6000000e+002
//   )

//   property("fbias") = forAll(Gen.choose(1,25)) { f =>
//     val helper = Helper("cec2005")
//     helper.fbiasFromResource(f) === fbiases(f - 1)
//   }

//   property("shift from resource") = {
//     val helper = Helper("cec2005")
//     val resource = "ackley_func_data.txt"
//     val shift = helper.shiftFromResource[nat._2](resource)
//     shift.toList === List(-1.6823000e+001, 1.4976900e+001)
//   }

//   property("matrices from resource") = {
//     val helper = Helper("cec2013/niching")
//     val resource = "CF3_M_D2.dat"
//     val matrices: Dimension[nat._6,Matrix[nat._2,nat._2,Double]] = helper.matricesFromResource[nat._6,nat._2](resource)

//     val m = Matrix.wrap[nat._2,nat._2,Double](
//       Vector(8.0594686124723847e-01, 5.9198788572548056e-01),
//       Vector(5.9198788572548056e-01, -8.0594686124723836e-01)
//     )

//     val n = Matrix.wrap[nat._2,nat._2,Double](
//       Vector(-8.8719803469714120e-01, -4.6138882434399231e-01),
//       Vector(-4.6138882434399664e-01, 8.8719803469713754e-01)
//     ).t

//     matrices.head === m &&
//     matrices.last === n &&
//     matrices.size === 6
//   }

// }
