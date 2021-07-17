// package benchmarks
// package bbob2009

// import org.scalacheck._
// import org.scalacheck.Prop._

// import shapeless._
// import shapeless.nat._
// import spire.implicits._

// import Benchmarks._
// import benchmarks.dimension._
// import benchmarks.dimension.Generators._
// import benchmarks.matrix._

// object BenchmarksTest extends Properties("BBOB2009 Benchmarks") {

//   val p1 = Sized(0.0, 0.0)
//   val p2 = Sized(1.0, 2.0)

//   property("penalty") = forAll(genSized(-100.0, 100.0)) { x =>
//     penalty(x) >= 0.0
//   } && {
//     val y = Sized(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
//     penalty(y) === 55.0
//   }

//   property("f1") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F1Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0)
//     }
//     f1(x) >= 0.0
//   } && {
//     implicit val p = new F1Params[_2,Double] {
//       val params = (Sized(0.2528, -1.1568), 79.48)
//     }
//     f1(p1) === 80.88209408 &&
//     f1(p2) === 90.00369408
//   }

//   property("f2") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F2Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0)
//     }
//     f2(x) >= 0.0
//   } && {
//     implicit val p = new F2Params[_2,Double] {
//       val params = (Sized(1.2072, 0.448), -209.88)
//     }
//     f2(p1) === 207486.72423501048 &&
//     f2(p2) === 2125570.342739224
//   }

//   property("f3") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F3Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0)
//     }
//     f3(x) >= 0.0
//   } && {
//     implicit val p = new F3Params[_2,Double] {
//       val params = (Sized(-2.3408, 2.3), -462.09)
//     }
//     f3(p1) === -383.06427743867573 &&
//     f3(p2) === -440.5552734899663
//   }

//   property("f4") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F4Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0)
//     }
//     f4(x) >= 0.0
//   } && {
//     implicit val p = new F4Params[_2,Double] {
//       val params = (Sized(-2.3408, 2.3), -462.09)
//     }
//     f4(p1) === 212.45714632070013 &&
//     f4(p2) === 612.5233204741321
//   }

//   property("f5") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F5Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0)
//     }
//     f5(x) >= 0.0
//   } && {
//     implicit val p = new F5Params[_2,Double] {
//       val params = (Sized(3.0592, 2.0864), -9.21)
//     }
//     f5(p1) === 45.79 &&
//     f5(p2) === 24.79
//   }

//   property("f6") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F6Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0,
//         Matrix.eye[_2,Double], Matrix.eye[_2,Double]
//       )
//     }
//     f6(x) >= 0.0
//   } && {
//     implicit val p = new F6Params[_2,Double] {
//       val params = (
//         Sized(2.7816, 1.1136), 35.9,
//         Matrix.wrap[_2,_2,Double](
//           Vector(0.994567,-0.104096),
//           Vector(-0.104096,-0.994567)
//         ),
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.329834,0.944039),
//           Vector(0.944039,0.329834)
//         )
//       )
//     }
//     f6(p1) === 228346.02149915518 &&
//     f6(p2) === 63279.64024377276
//   }

//   property("f7") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F7Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0,
//         Matrix.eye[_2,Double], Matrix.eye[_2,Double]
//       )
//     }
//     f7(x) >= 0.0
//   } && {
//     implicit val p = new F7Params[_2,Double] {

//       val params = (
//         Sized(-0.2256, 0.736), 92.94,
//         Matrix.wrap[_2,_2,Double](
//           Vector(0.599293, 0.80053),
//           Vector(0.80053, -0.599293)
//         ),
//         Matrix.wrap[_2,_2,Double](
//           Vector(0.34534, 0.938478),
//           Vector(-0.938478, 0.34534)
//         )
//       )
//     }
//     f7(p1) === 100.37086308732489 &&
//     f7(p2) === 238.2699941871949
//   }

//   property("f8") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F8Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0)
//     }
//     f8(x) >= 0.0
//   } && {
//     implicit val p = new F8Params[_2,Double] {
//       val params = (Sized(-0.0552, -0.3708), 149.15)
//     }
//     f8(p1) === 155.776101642076185 &&
//     f8(p2) === 223.032372285276267
//   }

//   property("f9") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F9Params[_2,Double] {
//       val params = (0.0, Matrix.eye[_2,Double])
//     }
//     f9(x) >= 0.0
//   } && {
//     implicit val p = new F9Params[_2,Double] {
//       val params = (
//         123.83,
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.736528, 0.676407),
//           Vector(0.676407, 0.736528)
//         )
//       )
//     }
//     f9(p1) === 130.32999999999998 &&
//     f9(p2) === 320.787855694743
//   }

//   property("f10") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F10Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0, Matrix.eye[_2,Double])
//     }
//     f10(x) >= 0.0
//   } && {
//     implicit val p = new F10Params[_2,Double] {
//       val params = (
//         Sized(-1.7264, -1.508), -54.94,
//         Matrix.wrap[_2,_2,Double](
//           Vector(0.977332, 0.211713),
//           Vector(-0.211713, 0.977332)
//         )
//       )
//     }
//     f10(p1) === 3012725.1531188088 &&
//     f10(p2) === 1.5993611909160418E7
//   }

//   property("f11") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F11Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0, Matrix.eye[_2,Double])
//     }
//     f11(x) >= 0.0
//   } && {
//     implicit val p = new F11Params[_2,Double] {
//       val params = (
//         Sized(-0.9384, -3.1504), 76.27,
//         Matrix.wrap[_2,_2,Double](
//           Vector(0.30156, 0.953447),
//           Vector(0.953447, -0.30156)
//         )
//       )
//     }
//     f11(p1) === 1.0191384843712414E7 &&
//     f11(p2) === 2.9635303873962525E7
//   }

//   property("f12") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F12Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0, Matrix.eye[_2,Double])
//     }
//     f12(x) >= 0.0
//   } && {
//     implicit val p = new F12Params[_2,Double] {
//       val params = (
//         Sized(-0.892, 3.9912), -621.11,
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.999997, 0.002485),
//           Vector(-0.002485, -0.999997)
//         )
//       )
//     }
//     f12(p1) === 2.5380326113958028E8 &&
//     f12(p2) === 1.0605291041185623E7
//   }

//   property("f13") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F13Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0,
//         Matrix.eye[_2,Double], Matrix.eye[_2,Double]
//       )
//     }
//     f13(x) >= 0.0
//   } && {
//     implicit val p = new F13Params[_2,Double] {
//       val params = (
//         Sized(0.8744, -1.704), 29.97,
//         Matrix.wrap[_2,_2,Double](
//           Vector(0.827751, -0.561095),
//           Vector(-0.561095, -0.827751)
//         ),
//         Matrix.wrap[_2,_2,Double](
//           Vector(0.487699, 0.873012),
//           Vector(0.873012, -0.487699)
//         )
//       )
//     }
//     f13(p1) === 401.5200275285964 &&
//     f13(p2) === 322.16746069929945
//   }

//   property("f14") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F14Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0, Matrix.eye[_2,Double])
//     }
//     f14(x) >= 0.0
//   } && {
//     implicit val p = new F14Params[_2,Double] {
//       val params = (
//         Sized(-0.872, -1.2448), -52.35,
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.965248, -0.261337),
//           Vector(-0.261337, 0.965248)
//         )
//       )
//     }
//     f14(p1) === -50.86208518611569 &&
//     f14(p2) === -33.70140542917638
//   }

//   property("f15") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F15Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0,
//         Matrix.eye[_2,Double], Matrix.eye[_2,Double]
//       )
//     }
//     f15(x) >= 0.0
//   } && {
//     implicit val p = new F15Params[_2,Double] {
//       val params = (
//         Sized(-3.0568, 3.0016), 1000.0,
//         Matrix.wrap[_2,_2,Double](
//           Vector(0.968054, -0.250741),
//           Vector(-0.250741, -0.968054)
//         ),
//         Matrix.wrap[_2,_2,Double](
//           Vector(0.950193, 0.311664),
//           Vector(0.311664, -0.950193)
//         )
//       )
//     }
//     f15(p1) === 1079.9262128908133 &&
//     f15(p2) === 1046.3248505990719
//   }

//   property("f16") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F16Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0,
//         Matrix.eye[_2,Double], Matrix.eye[_2,Double]
//       )
//     }
//     f16(x) >= 0.0
//   } && {
//     implicit val p = new F16Params[_2,Double] {
//       val params = (
//         Sized(1.8328, -2.1424), 71.35,
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.010416, -0.999946),
//           Vector(-0.999946, 0.010416)
//         ),
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.999659, -0.026112),
//           Vector(0.026112, -0.999659)
//         )
//       )
//     }
//     f16(p1) === 146.90148956646777 &&
//     f16(p2) === 100.61590510903495
//   }

//   property("f17") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F17Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0,
//         Matrix.eye[_2,Double], Matrix.eye[_2,Double]
//       )
//     }
//     f17(x) >= 0.0
//   } && {
//     implicit val p = new F17Params[_2,Double] {
//       val params = (
//         Sized(3.656, 2.5496), -16.94,
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.94697, -0.321322),
//           Vector(0.321322, -0.94697)
//         ),
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.127842, -0.991795),
//           Vector(-0.991795, 0.127842)
//         )
//       )
//     }
//     f17(p1) === 23.800347975772173 &&
//     f17(p2) === 5.904389055594255
//   }

//   property("f18") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F18Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0,
//         Matrix.eye[_2,Double], Matrix.eye[_2,Double]
//       )
//     }
//     f18(x) >= 0.0
//   } && {
//     implicit val p = new F18Params[_2,Double] {
//       val params = (
//         Sized(3.656, 2.5496), -16.94,
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.94697, -0.321322),
//           Vector(0.321322, -0.94697)
//         ),
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.127842, -0.991795),
//           Vector(-0.991795, 0.127842)
//         )
//       )
//     }
//     f18(p1) === 1258.5489743857706 &&
//     f18(p2) === 642.7620301505375
//   }

//   property("f19") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F19Params[_2,Double] {
//       val params = (0.0, Matrix.eye[_2,Double])
//     }
//     f19(x) >= 0.0
//   } && {
//     implicit val p = new F19Params[_2,Double] {
//       val params = (
//         -102.55,
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.82929, 0.558818),
//           Vector(0.558818, 0.82929)
//         )
//       )
//     }
//     f19(p1) === -102.29962625728024 &&
//     f19(p2) === -100.25186697419281
//   }

//   property("f20") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F20Params[_2,Double] {
//       val params = (0.0, Sized(1.0, 1.0))
//     }
//     f20(x) >= 0.0
//   } && {
//     implicit val p = new F20Params[_2,Double] {
//       val params = (-546.5, Sized(-1.0, 1.0))
//     }
//     f20(p1) === 4975.015399746393 &&
//     f20(p2) === -541.8410374659132
//   }

//   // f21 & f22 have not been tested.
//   // The randomness makes it difficult to verify against the original C code.

//   property("f23") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F23Params[_2,Double] {
//       val params = (Sized(0.0, 0.0), 0.0,
//         Matrix.eye[_2,Double], Matrix.eye[_2,Double]
//       )
//     }
//     f23(x) >= 0.0
//   } && {
//     implicit val p = new F23Params[_2,Double] {
//       val params = (
//         Sized(2.7672, 2.1248), 6.87,
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.884846, 0.465885),
//           Vector(-0.465885, -0.884846)
//         ),
//         Matrix.wrap[_2,_2,Double](
//           Vector(0.797182, 0.603739),
//           Vector(-0.603739, 0.797182)
//         )
//       )
//     }

//     f23(p1) === 31.684643141172447 &&
//     f23(p2) === 69.88139433627418
//   }

//   property("f24") = forAll(gen2(-5.0, 5.0)) { x =>
//     implicit val p = new F24Params[_2,Double] {
//       val params = (0.0, Sized(1.0, 1.0),
//         Matrix.eye[_2,Double], Matrix.eye[_2,Double]
//       )
//     }
//     f24(x) >= 0.0
//   } && {
//     implicit val p = new F24Params[_2,Double] {
//       val params = (102.61, Sized(-1.0, 1.0),
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.115883, 0.993263),
//           Vector(0.993263, 0.115883)
//         ),
//         Matrix.wrap[_2,_2,Double](
//           Vector(-0.892201, 0.451638),
//           Vector(0.451638, 0.892201)
//         )
//       )
//     }

//     f24(p1) === 142.06530570217677 &&
//     f24(p2) === 144.80265420862628
//   }

// }
