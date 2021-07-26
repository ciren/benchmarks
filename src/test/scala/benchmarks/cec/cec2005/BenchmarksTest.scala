package benchmarks
package cec
package cec2005

import zio.prelude._
import zio.test._
import spire.math.{abs}
import spire.implicits._

import cilib.{RVar, RNG}
import Generators._
import benchmarks.cec.cec2005.Benchmarks._

object CEC2005BenchmarkTest extends DefaultRunnableSpec {

  val rng0 = RNG.init(0)

  def validate[F[+_]](
    f: F[Double] => Double,
    x: F[Double],
    bias: Double,
    y: (Double, Double),
    point: F[Double]
  ) =
    assert(f(x))(Assertion.isGreaterThanEqualTo(bias)) &&
    assert(abs(y._1 - f(point)))(Assertion.isLessThanEqualTo(10.0 ** -y._2))


  def validateR[F[+_]](
    f: F[Double] => RVar[Double],
    x: F[Double],
    bias: Double,
    y: (Double, Double),
    point: F[Double]
  ) =
    assert(f(x).eval(rng0))(Assertion.isGreaterThanEqualTo(bias)) &&
    assert(abs(y._1 - f(point).eval(rng0)))(Assertion.isLessThanEqualTo(10.0 ** -y._2))


  def toAtLeast2List(x: NonEmptyList[Double]): AtLeast2List[Double] =
    AtLeast2List.make(x) match {
      case ZValidation.Success(_, v) => v
      case ZValidation.Failure(_, e) => sys.error(e.toString())
    }

  val point2 = NonEmptyList(0.0, 1.0)
  val point10 = NonEmptyList.fromIterableOption((0 until 10).map(_.toDouble)).get
  val point30 = NonEmptyList.fromIterableOption((0 until 30).map(_.toDouble)).get
  val point50 = NonEmptyList.fromIterableOption((0 until 50).map(_.toDouble)).get

  def spec: ZSpec[Environment,Failure] = suite("CEC2005 Benchmarks")(

// object BenchmarksTest extends Properties("CEC2005 Benchmarks") {

//   implicit val f17ParamsNoNoise = new F17Params[Double] {
//     val params = (helper.fbiasFromResource(17), RVar.point(0.0))
//   }
//   implicit def f24ParamsNoNoise[N<:Nat:CECSized](implicit ev: ToInt[N]) =
//     new F24Params[N,Double] {
//       val params = (
//         Sized.wrap(helper.shiftsFromResource("hybrid_func4_data.txt").toVector),
//         helper.matrix10FromResource(s"hybrid_func4_M_D${ev.apply}.txt"),
//         helper.fbiasFromResource(24),
//         RVar.point(0.0)
//       )
//     }

    //   property("F1") = forAll(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
    //     implicit val fbias = new FBias { val fbias = -450.0 }
    //     validate[_2] (f1, s2,  (4447.8239016200005, 20)) &&
    //     validate[_10](f1, s10, (27858.83407531,     20)) &&
    //     validate[_30](f1, s30, (93750.44521420001,  20)) &&
    //     validate[_50](f1, s50, (188185.93987866005, 10))
    //   }
    testM("F1") {
      val bias = -450.0
      check(genCECSized(-100, 100)) {
        case (s2, s10, s30, s50) =>
          validate(f1[Double], s2,  bias, (4447.8239016200005, 20), point2) &&
          validate(f1[Double], s10, bias, (27858.83407531,     20), point10) &&
          validate(f1[Double], s30, bias, (93750.44521420001,  20), point30) &&
          validate(f1[Double], s50, bias, (188185.93987866005, 10), point50)
      }
    },
    //   property("F2") = forAll(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
    //     implicit val fbias = new FBias { val fbias = -450.0 }
    //     validate[_2] (f2, s2,  (3150.7609202500003,  20)) &&
    //     validate[_10](f2, s10, (99459.43179384002,   20)) &&
    //     validate[_30](f2, s30, (4621314.251146632,   20)) &&
    //     validate[_50](f2, s50, (3.989686720149214E7,  8))
    //   }
    testM("F2") {
      val bias = -450.0
      check(genCECSized(-100.0, 100.0)) {
        case (s2, s10, s30, s50) =>
          validate(f2[Double], s2,  bias, (3150.7609202500003,  20), point2) &&
          validate(f2[Double], s10, bias, (99459.43179384002,   20), point10) &&
          validate(f2[Double], s30, bias, (4621314.251146632,   20), point30) &&
          validate(f2[Double], s50, bias, (3.989686720149214E7,  8), point50)
      }
    },
    //   property("F3") = forAll(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
    //     implicit val fbias = new FBias { val fbias = -450.0 }
    //     validate[_2] (f3, s2,  (3.270670729443313E9,  10)) &&
    //     validate[_10](f3, s10, (1.7626359593380923E9,  6)) &&
    //     validate[_30](f3, s30, (4.831191594054285E9,   5)) &&
    //     validate[_50](f3, s50, (2.7057856612503826E10, 4))
    //   }
    testM("F3") {
      val f: NonEmptyList[Double] => Double = x => f3(toAtLeast2List(x))
      println(s"point eval: ${3.270670729443313E9 - f(point2)}")
      val bias = -450.0

      check(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
        validate(f, s2,  bias, (3.270670729443313E9,  10), point2) &&
        validate(f, s10, bias, (1.7626359593380923E9,  6), point10) &&
        validate(f, s30, bias, (4.831191594054285E9,   5), point30) &&
        validate(f, s50, bias, (2.7057856612503826E10, 4), point50)
      }
    },
//   property("F4") = forAll(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = -450.0 }
//     validateR[_2] (f4, s2,  (3150.7609202500002,  20)) &&
//     validateR[_10](f4, s10, (99459.43179384002,   20)) &&
//     validateR[_30](f4, s30, (4621314.251146632 ,  20)) &&
//     validateR[_50](f4, s50, (3.989686720149214E7, 20))
//   }
    testM("F4") {
      val bias = -450.0
      def f(x: NonEmptyList[Double]) = f4Noise(x, RVar.point(0.0))

      check(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
        validateR(f, s2,  bias, (3150.7609202500002,  20), point2) &&
        validateR(f, s10, bias, (99459.43179384002,   20), point10) &&
        validateR(f, s30, bias, (4621314.251146632 ,  20), point30) &&
        validateR(f, s50, bias, (3.989686720149214E7, 20), point50)
      }
    },

//   property("F5") = forAll(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = -310.0 }
//     validate[_2] (f5, s2,  (5818.0,            20)) &&
//     validate[_10](f5, s10, (24810.7801,        20)) &&
//     validate[_30](f5, s30, (62241.8054,        20)) &&
//     validate[_50](f5, s50, (65193.47299999998, 20))
//   }
    testM("F5") {
      val bias = -310.0

       check(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
         validate(f5[Double], s2,  bias, (5818.0,            20), point2) &&
         validate(f5[Double], s10, bias, (24810.7801,        20), point10) &&
         validate(f5[Double], s30, bias, (62241.8054,        20), point30) &&
         validate(f5[Double], s50, bias, (65193.47299999998, 20), point50)
      }
    },

//   property("F6") = forAll(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 390.0 }
//     validate[_2] (f6, s2,  (4.0364713289297366E9,  20)) &&
//     validate[_10](f6, s10, (1.553036764175096E10,  20)) &&
//     validate[_30](f6, s30, (6.717859853500358E10,  20)) &&
//     validate[_50](f6, s50, (1.3641793823548611E11, 20))
//   }
    testM("F6") {
      val bias = 390.0
      val f: NonEmptyList[Double] => Double = x => f6(toAtLeast2List(x))

      check(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
        validate(f, s2,  bias, (4.0364713289297366E9,  20), point2) &&
        validate(f, s10, bias, (1.553036764175096E10,  20), point10) &&
        validate(f, s30, bias, (6.717859853500358E10,  20), point30) &&
        validate(f, s50, bias, (1.3641793823548611E11, 20), point50)
      }
    }

//   property("F7") = forAll(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = -180.0 }
//     validate[_2] (f7, s2,  (-139.95133241089783, 20)) &&
//     validate[_10](f7, s10, (1118.9644044344782,  10)) &&
//     validate[_30](f7, s30, (5040.005979134262,   20)) &&
//     validate[_50](f7, s50, (7123.007406890281,   10))
//   }

//   property("F8") = forAll(genCECSized(-32.0, 32.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = -140.0 }
//     validate[_2] (f8, s2,  (-118.09381183982995, 20)) &&
//     validate[_10](f8, s10, (-118.19517272442779, 20)) &&
//     validate[_30](f8, s30, (-118.24337823113312, 10)) &&
//     validate[_50](f8, s50, (-118.34589514893673, 10))
//   }

//   property("F9") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = -330.0 }
//     validate[_2] (f9, s2,  (-298.72809507082457, 20)) &&
//     validate[_10](f9, s10, (229.3569160579391,   20)) &&
//     validate[_30](f9, s30, (9284.964421232971,   20)) &&
//     validate[_50](f9, s50, (43417.685263889995,  20))
//   }

//   property("F10") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = -330.0 }
//     validate[_2] (f10, s2,  (-258.3693304124196, 20)) &&
//     validate[_10](f10, s10, (844.6174944770778,  20)) &&
//     validate[_30](f10, s30, (22858.67302958929,  10)) &&
//     validate[_50](f10, s50, (108602.16406402343, 10))
//   }

//   property("F11") = forAll(genCECSized(-0.5, 0.5)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 90.0 }
//     validate[_2] (f11, s2,  (94.73273623129674, 20)) &&
//     validate[_10](f11, s10, (107.80172911710315, 7)) &&
//     validate[_30](f11, s30, (141.67918424338953, 7)) &&
//     validate[_50](f11, s50, (194.75188189582158, 7))
//   }

//   property("F12") = forAll(genCECSized(-pi, pi)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = -460.0 }
//     validate[_2] (f12, s2,  (-58.812048337208671,   1)) &&
//     validate[_10](f12, s10, (415454.122431980236,   0)) &&
//     validate[_30](f12, s30, (1697136.1208056952,   20)) &&
//     validate[_50](f12, s50, (1.0964401544490915E7, 20))
//   }

//   property("F13") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = -130.0 }
//     validate[_2] (f13, s2,  (7316.601742858467,   10)) &&
//     validate[_10](f13, s10, (4.915053571899814E8, 20)) &&
//     validate[_30](f13, s30, (5.486102447250848E12, 3)) &&
//     validate[_50](f13, s50, (5.599844313220846E14, 1))
//   }

//   property("F14") = forAll(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = -300.0 }
//     validate[_2] (f14, s2,  (-298.9995366425229,  20)) &&
//     validate[_10](f14, s10, (-294.96422178056105, 20)) &&
//     validate[_30](f14, s30, (-284.7094034669994,  20)) &&
//     validate[_50](f14, s50, (-275.36165637403394, 20))
//   }

//   property("F15") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 120.0 }
//     validate[_2] (f15, s2,  (1670.3098499428042,  1)) &&
//     validate[_10](f15, s10, (2169.1174006125125, 20)) &&
//     validate[_30](f15, s30, (23168.72372759631,  20)) &&
//     validate[_50](f15, s50, (2593.5462374358854, 20))
//   }

//   property("F16") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 120.0 }
//     validate[_2] (f16, s2,  (1024.4563203828557,  1)) &&
//     validate[_10](f16, s10, (2561.0729169218944, 10)) &&
//     validate[_30](f16, s30, (21102.818649611272, 20)) &&
//     validate[_50](f16, s50, (3445.060807983211,   8))
//   }

//   property("F17") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 120.0 }
//     validateR[_2] (f17, s2,  (1024.4563203828557,  1)) &&
//     validateR[_10](f17, s10, (2561.0729169218944, 10)) &&
//     validateR[_30](f17, s30, (21102.818649611272, 20)) &&
//     validateR[_50](f17, s50, (3445.060807983211,   8))
//   }

//   property("F18") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 10.0 }
//     validate[_2] (f18, s2,  (1935.5555316067705, 20)) &&
//     validate[_10](f18, s10, (2541.8953999043965, 10)) &&
//     validate[_30](f18, s30, (5671.934704451331,  20)) &&
//     validate[_50](f18, s50, (22121.867767800537, 11))
//   }

//   property("F19") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 10.0 }
//     validate[_2] (f19, s2,  (1943.5102449172944, 20)) &&
//     validate[_10](f19, s10, (2542.029239979566,   5)) &&
//     validate[_30](f19, s30, (5671.934704451331,  20)) &&
//     validate[_50](f19, s50, (22121.867767800537,  5))
//   }

//   property("F20") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 10.0 }
//     validate[_2] (f20, s2,  (1943.2149305978148, 10)) &&
//     validate[_10](f20, s10, (2540.5074257131655, 10)) &&
//     validate[_30](f20, s30, (5671.934704451331,  10)) &&
//     validate[_50](f20, s50, (22121.867767800533, 10))
//   }

//   property("F21") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 360.0 }
//     validate[_2] (f21, s2,  (1807.6492408710099,  10)) &&
//     validate[_10](f21, s10, (2913.6087022433276,  20)) &&
//     validate[_30](f21, s30, (1584636.933825271,    8)) &&
//     validate[_50](f21, s50, (6.383059946630113E8,  5))
//   }

//   property("F22") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 360.0 }
//     validate[_2] (f22, s2,  (3668.2321698175397, 20)) &&
//     validate[_10](f22, s10, (2395.986380515197,   6)) &&
//     validate[_30](f22, s30, (1.174879408800453E7, 7)) &&
//     validate[_50](f22, s50, (8.499062700185089E8, 5))
//   }

//   property("F23") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 360.0 }
//     validate[_2] (f23, s2,  (1807.6492408710099, 10)) &&
//     validate[_10](f23, s10, (2913.6087022433276, 20)) &&
//     validate[_30](f23, s30, (1584636.933825271,   8)) &&
//     validate[_50](f23, s50, (6.383059946630113E8, 6))
//   }

//   property("F24") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 360.0 }
//     validateR[_2] (f24, s2,  (1600.6959008838132, 20)) &&
//     validateR[_10](f24, s10, (4741.98275706078,   10)) &&
//     validateR[_30](f24, s30, (103828.33658202366, 10)) &&
//     validateR[_50](f24, s50, (6559057.724458647,  7))
//   }

//   property("F25") = forAll(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
//     implicit val fbias = new FBias { val fbias = 260.0 }
//     validateR[_2] (f25, s2,  (1600.6959008838132, 20)) &&
//     validateR[_10](f25, s10, (4741.98275706078,   10)) &&
//     validateR[_30](f25, s30, (103828.33658202366, 10)) &&
//     validateR[_50](f25, s50, (6559057.724458647,  7))
//   }
  )
}
