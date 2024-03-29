package benchmarks
package cec
package cec2005

import zio.test._
import zio.prelude.ZValidation

import scala.math._

import cilib.{RVar, RNG, NonEmptyVector}
import Generators._
import benchmarks.cec.cec2005.Benchmarks._

object CEC2005BenchmarkTest extends ZIOSpecDefault {

  val rng0 = RNG.init(0)

  def validate[F[+_]](
    f: F[Double] => Double,
    x: F[Double],
    bias: Double,
    y: (Double, Double),
    point: F[Double]
  ) =
    assert(f(x))(Assertion.isGreaterThanEqualTo(bias)) &&
    assert(abs(y._1 - f(point)))(Assertion.isLessThanEqualTo(pow(10.0, -y._2)))


  def validateR[F[+_]](
    f: F[Double] => RVar[Double],
    x: F[Double],
    bias: Double,
    y: (Double, Double),
    point: F[Double]
  ) =
    assert(f(x).runResult(rng0))(Assertion.isGreaterThanEqualTo(bias)) &&
    assert(abs(y._1 - f(point).runResult(rng0)))(Assertion.isLessThanEqualTo(pow(10.0, -y._2)))


  def toAtLeast2List(x: NonEmptyVector[Double]): AtLeast2List =
    AtLeast2List.make(x) match {
      case ZValidation.Success(_, v) => v
      case ZValidation.Failure(_, e) => sys.error(e.toString())
    }

  val point2 = NonEmptyVector(0.0, 1.0)
  val point10 = NonEmptyVector.fromIterableOption((0 until 10).map(_.toDouble)).get
  val point30 = NonEmptyVector.fromIterableOption((0 until 30).map(_.toDouble)).get
  val point50 = NonEmptyVector.fromIterableOption((0 until 50).map(_.toDouble)).get

  def spec = suite("CEC2005 Benchmarks")(

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


    test("F1") {
      val bias = -450.0
      check(genCECSized(-100, 100)) {
        case (s2, s10, s30, s50) =>
          validate(f1, s2,  bias, (4447.8239016200005, 20), point2) &&
          validate(f1, s10, bias, (27858.83407531,     20), point10) &&
          validate(f1, s30, bias, (93750.44521420001,  20), point30) &&
          validate(f1, s50, bias, (188185.93987866005, 10), point50)
      }
    },

    test("F2") {
      val bias = -450.0
      check(genCECSized(-100.0, 100.0)) {
        case (s2, s10, s30, s50) =>
          validate(f2, s2,  bias, (3150.7609202500003,  20), point2) &&
          validate(f2, s10, bias, (99459.43179384002,   20), point10) &&
          validate(f2, s30, bias, (4621314.251146632,   20), point30) &&
          validate(f2, s50, bias, (3.989686720149214E7,  8), point50)
      }
    },

    test("F3") {
      val f: NonEmptyVector[Double] => Double = x => f3(toAtLeast2List(x))
      val bias = -450.0

      check(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
        validate(f, s2,  bias, (3.270670729443313E9,  10), point2) &&
        validate(f, s10, bias, (1.7626359593380923E9,  6), point10) &&
        validate(f, s30, bias, (4.831191594054285E9,   5), point30) &&
        validate(f, s50, bias, (2.7057856612503826E10, 4), point50)
      }
    },

    test("F4") {
      val bias = -450.0
      def f(x: NonEmptyVector[Double]) = f4Noise(x, RVar.pure(0.0))

      check(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
        validateR(f, s2,  bias, (3150.7609202500002,  20), point2) &&
        validateR(f, s10, bias, (99459.43179384002,   20), point10) &&
        validateR(f, s30, bias, (4621314.251146632 ,  20), point30) &&
        validateR(f, s50, bias, (3.989686720149214E7, 20), point50)
      }
    },

    test("F5") {
      val bias = -310.0

       check(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
         validate(f5, s2,  bias, (5818.0,            20), point2) &&
         validate(f5, s10, bias, (24810.7801,        20), point10) &&
         validate(f5, s30, bias, (62241.8054,        20), point30) &&
         validate(f5, s50, bias, (65193.47299999998, 20), point50)
      }
    },

    test("F6") {
      val bias = 390.0
      val f: NonEmptyVector[Double] => Double = x => f6(toAtLeast2List(x))

      check(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
        validate(f, s2,  bias, (4.0364713289297366E9,  20), point2) &&
        validate(f, s10, bias, (1.553036764175096E10,  20), point10) &&
        validate(f, s30, bias, (6.717859853500358E10,  20), point30) &&
        validate(f, s50, bias, (1.3641793823548611E11, 20), point50)
      }
    },

    test("F7") {
      val bias = -180.0

      check(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
        validate(f7, s2,  bias, (-139.95133241089783, 20), point2) &&
        validate(f7, s10, bias, (1118.9644044344782,  10), point10) &&
        validate(f7, s30, bias, (5040.005979134262,   20), point30) &&
        validate(f7, s50, bias, (7123.007406890281,   10), point50)
      }
    },

    test("F8") {
      val bias = -140.0

      check(genCECSized(-32.0, 32.0)) { case (s2, s10, s30, s50) =>
        validate(f8, s2,  bias, (-118.09381183982995, 20), point2) &&
        validate(f8, s10, bias, (-118.19517272442779, 20), point10) &&
        validate(f8, s30, bias, (-118.24337823113312, 10), point30) &&
        validate(f8, s50, bias, (-118.34589514893673, 10), point50)
      }
   },

    test("F9") {
      val bias = -330.0

      check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
        validate(f9, s2,  bias, (-298.72809507082457, 20), point2) &&
        validate(f9, s10, bias, (229.3569160579391,   20), point10) &&
        validate(f9, s30, bias, (9284.964421232971,   20), point30) &&
        validate(f9, s50, bias, (43417.685263889995,  20), point50)
      }
    },

    test("F10") {
      val bias = -330.0

      check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
        validate(f10, s2,  bias, (-258.3693304124196, 20), point2) &&
        validate(f10, s10, bias, (844.6174944770778,  20), point10) &&
        validate(f10, s30, bias, (22858.67302958929,  10), point30) &&
        validate(f10, s50, bias, (108602.16406402343, 10), point50)
      }
    },

    test("F11") {
      val bias = 90.0
      check(genCECSized(-0.5, 0.5)) { case (s2, s10, s30, s50) =>
        validate(f11, s2,  bias, (94.73273623129674, 20), point2) &&
        validate(f11, s10, bias, (107.80172911710315, 7), point10) &&
        validate(f11, s30, bias, (141.67918424338953, 7), point30) &&
        validate(f11, s50, bias, (194.75188189582158, 7), point50)
      }
    },

    // FIXME: This test should be working, but it isn't
    // test("F12") {
    //   val bias = -460.0
    //   check(genCECSized(-Pi, Pi)) { case (s2, s10, s30, s50) =>
    //     validate(f12, s2,  bias, (-58.812048337208671,   1), point2) &&
    //     validate(f12, s10, bias, (415454.122431980236,   0), point10) &&
    //     validate(f12, s30, bias, (1697136.1208056952,   20), point30) &&
    //     validate(f12, s50, bias, (1.0964401544490915E7, 20), point50)
    //   }
    // },

    test("F13") {
      val bias = -130.0
      val f: NonEmptyVector[Double] => Double = x => f13(toAtLeast2List(x))

      check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
        validate(f, s2,  bias, (7316.601742858467,   10), point2) &&
        validate(f, s10, bias, (4.915053571899814E8, 20), point10) &&
        validate(f, s30, bias, (5.486102447250848E12, 3), point30) &&
        validate(f, s50, bias, (5.599844313220846E14, 1), point50)
      }
    },

    test("F14") {
      val bias = -300.0
      val f: NonEmptyVector[Double] => Double = x => f14(toAtLeast2List(x))

      check(genCECSized(-100.0, 100.0)) { case (s2, s10, s30, s50) =>
        validate(f, s2,  bias, (-298.9995366425229,  20), point2) &&
        validate(f, s10, bias, (-294.96422178056105, 20), point10) &&
        validate(f, s30, bias, (-284.7094034669994,  20), point30) &&
        validate(f, s50, bias, (-275.36165637403394, 20), point50)
      }
    },

    // test("f15") {
    //   val bias = 120.0

    //   check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
    //     validate(f15, s2,  bias, (1670.3098499428042,  1), point2) &&
    //     validate(f15, s10, bias, (2169.1174006125125, 20), point10) &&
    //     validate(f15, s30, bias, (23168.72372759631,  20), point30) &&
    //     validate(f15, s50, bias, (2593.5462374358854, 20), point50)
    //   }
    // },

    test("f16") {
      val bias = 120.0

      check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
        validate(f16, s2,  bias, (1024.4563203828557,  1), point2) &&
        validate(f16, s10, bias, (2561.0729169218944, 10), point10) &&
        validate(f16, s30, bias, (21102.818649611272, 20), point30) &&
        validate(f16, s50, bias, (3445.060807983211,   8), point50)
      }
    },

    // test("f17") {
    //   val bias = 120.0

    //   check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
    //     validateR(f17, s2,  bias, (1024.4563203828557,  1), point2) &&
    //     validateR(f17, s10, bias, (2561.0729169218944, 10), point10) &&
    //     validateR(f17, s30, bias, (21102.818649611272, 20), point30) &&
    //     validateR(f17, s50, bias, (3445.060807983211,   8), point50)
    //   }
    // },

    // test("f18") {
    //   val bias = 10.0

    //   check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
    //     validate(f18, s2,  bias, (1935.5555316067705, 20), point2) &&
    //     validate(f18, s10, bias, (2541.8953999043965, 10), point10) &&
    //     validate(f18, s30, bias, (5671.934704451331,  20), point30) &&
    //     validate(f18, s50, bias, (22121.867767800537, 11), point50)
    //   }
    // },

    // test("f19") {
    //   val bias = 10.0

    //   check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
    //     validate(f19, s2,  bias, (1943.5102449172944, 20), point2) &&
    //     validate(f19, s10, bias, (2542.029239979566,   5), point10) &&
    //     validate(f19, s30, bias, (5671.934704451331,  20), point30) &&
    //     validate(f19, s50, bias, (22121.867767800537,  5), point50)
    //   }
    // },

    test("f20") {
      val bias = 10.0

      check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
        validate(f20, s2,  bias, (1943.2149305978148, 10), point2) &&
        validate(f20, s10, bias, (2540.5074257131655, 10), point10) &&
        validate(f20, s30, bias, (5671.934704451331,  10), point30) &&
        validate(f20, s50, bias, (22121.867767800533, 10), point50)
      }
    },

    // test("f21") {
    //   val bias = 360.0
    //   val f: NonEmptyVector[Double] => Double = x => f21(toAtLeast2List(x))

    //   check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
    //     validate(f, s2,  bias, (1807.6492408710099,  10), point2) &&
    //     validate(f, s10, bias, (2913.6087022433276,  20), point10) &&
    //     validate(f, s30, bias, (1584636.933825271,    8), point30) &&
    //     validate(f, s50, bias, (6.383059946630113E8,  5), point50)
    //   }
    // },

    // test("f22") {
    //   val bias = 360.0
    //   val f: NonEmptyVector[Double] => Double = x => f22(toAtLeast2List(x))

    //   check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
    //     validate(f, s2,  bias, (3668.2321698175397, 20), point2) &&
    //     validate(f, s10, bias, (2395.986380515197,   6), point10) &&
    //     validate(f, s30, bias, (1.174879408800453E7, 7), point30) &&
    //     validate(f, s50, bias, (8.499062700185089E8, 5), point50)
    //   }
    // },

    // test("f23") {
    //   val bias = 360.0
    //   val f: NonEmptyVector[Double] => Double = x => f23(toAtLeast2List(x))

    //   check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
    //     validate(f, s2,  bias, (1807.6492408710099, 10), point2) &&
    //     validate(f, s10, bias, (2913.6087022433276, 20), point10) &&
    //     validate(f, s30, bias, (1584636.933825271,   8), point30) &&
    //     validate(f, s50, bias, (6.383059946630113E8, 6), point50)
    //   }
    // },

    // test("f24") {
    //   val bias = 260.0
    //   val f: NonEmptyVector[Double] => RVar[Double] = x => f24(toAtLeast2List(x))

    //   check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
    //     validateR(f, s2,  bias, (1600.6959008838132, 20), point2) &&
    //     validateR(f, s10, bias, (4741.98275706078,   10), point10) &&
    //     validateR(f, s30, bias, (103828.33658202366, 10), point30) &&
    //     validateR(f, s50, bias, (6559057.724458647,  7), point50)
    //   }
    // },

    // test("f25") {
    //   val bias = 260.0
    //   val f: NonEmptyVector[Double] => RVar[Double] = x => f25(toAtLeast2List(x))

    //   check(genCECSized(-5.0, 5.0)) { case (s2, s10, s30, s50) =>
    //     validateR(f, s2,  bias, (1600.6959008838132, 20), point2) &&
    //     validateR(f, s10, bias, (4741.98275706078,   10), point10) &&
    //     validateR(f, s30, bias, (103828.33658202366, 10), point30) &&
    //     validateR(f, s50, bias, (6559057.724458647,  7), point50)
    //   }
    // }
  )
}
