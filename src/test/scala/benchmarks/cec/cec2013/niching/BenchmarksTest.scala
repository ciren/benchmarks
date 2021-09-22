// package benchmarks
// package cec
// package cec2013
// package niching

// import org.scalacheck._
// import org.scalacheck.Prop._

// import benchmarks.dimension.Generators._
// import benchmarks.implicits._
// import Benchmarks._

// object BenchmarksTest extends Properties("CEC2013 Niching Benchmarks") {

//   property("f8") = forAll(gen2(0.0, 1.0)) { g =>
//     f8(g) >= -40.0 &&
//     f8(g) <= 0.0
//   }

//   property("f9") = forAll(gen2(-5.0, 5.0)) { g =>
//     f9(g) >= -2500.0 &&
//     f9(g) <= 0.0
//   }

//   property("f10") = forAll(gen2(-5.0, 5.0)) { g =>
//     f10(g) >= -2500.0 &&
//     f10(g) <= 0.0
//   }

//   property("f11") = forAll(gen2(-5.0, 5.0)) { g =>
//     f11(g) >= -4000.0 &&
//     f11(g) <= 0.0
//   }

//   property("f12") = forAll(gen2(-5.0, 5.0)) { g =>
//     f12(g) >= -4000.0 &&
//     f12(g) <= 0.0
//   }
// }
