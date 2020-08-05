package benchmarks
package cec
package cec2005

import benchmarks.cec.Helper
import benchmarks.dimension._
import benchmarks.dimension.implicits._
import benchmarks.matrix._
import benchmarks.matrix.implicits._
import shapeless._
import shapeless.ops.nat.ToInt
import spire.implicits._
import spire.math.{ ceil, floor }

import cilib.{ Dist, RVar }

trait F1Params[N <: Nat, A]  { val params: (Dimension[N, A], A)                       }
trait F2Params[N <: Nat, A]  { val params: (Dimension[N, A], A)                       }
trait F3Params[N <: Nat, A]  { val params: (Dimension[N, A], Matrix[N, N, A], A)      }
trait F4Params[N <: Nat, A]  { val params: (Dimension[N, A], A, RVar[A])              }
trait F5Params[N <: Nat, A]  { val params: (Dimension[N, A], Matrix[N, N, A], Double) }
trait F6Params[N <: Nat, A]  { val params: (Dimension[N, A], Double)                  }
trait F7Params[N <: Nat, A]  { val params: (Dimension[N, A], Matrix[N, N, A], A)      }
trait F8Params[N <: Nat, A]  { val params: (Dimension[N, A], Matrix[N, N, A], A)      }
trait F9Params[N <: Nat, A]  { val params: (Dimension[N, A], Double)                  }
trait F10Params[N <: Nat, A] { val params: (Dimension[N, A], Matrix[N, N, A], A)      }
trait F11Params[N <: Nat, A] { val params: (Dimension[N, A], Matrix[N, N, A], A)      }
trait F12Params[N <: Nat, A] {
  val params: (Dimension[N, A], Matrix[N, N, A], Matrix[N, N, A], A)
}
trait F13Params[N <: Nat, A] { val params: (Dimension[N, A], Double)             }
trait F14Params[N <: Nat, A] { val params: (Dimension[N, A], Matrix[N, N, A], A) }
trait F15Params[N <: Nat, A] {
  val params: (Dimension10[Dimension[N, A]], Dimension10[Matrix[N, N, A]], A)
}
trait F16Params[N <: Nat, A] {
  val params: (Dimension10[Dimension[N, A]], Dimension10[Matrix[N, N, A]], A)
}
trait F17Params[A] { val params: (A, RVar[A]) }
trait F18Params[N <: Nat, A] {
  val params: (Dimension10[Dimension[N, A]], Dimension10[Matrix[N, N, A]], A)
}
trait F19Params[A]           { val params: A                                 }
trait F20Params[N <: Nat, A] { val params: (Dimension10[Dimension[N, A]], A) }
trait F21Params[N <: Nat, A] {
  val params: (Dimension10[Dimension[N, A]], Dimension10[Matrix[N, N, A]], A)
}
trait F22Params[N <: Nat, A] {
  val params: (Dimension10[Dimension[N, A]], Dimension10[Matrix[N, N, A]], A)
}
trait F23Params[A] { val params: A }
trait F24Params[N <: Nat, A] {
  val params: (Dimension10[Dimension[N, A]], Dimension10[Matrix[N, N, A]], A, RVar[A])
}
trait F25Params[A] { val params: A }

sealed trait CECSized[N <: Nat]

trait Params {
  implicit object cecSized2  extends CECSized[_2]
  implicit object cecSized10 extends CECSized[_10]
  implicit object cecSized30 extends CECSized[_30]
  implicit object cecSized50 extends CECSized[_50]

  val helper = Helper("cec2005")

  implicit def f1Params[N <: Nat: ToInt: CECSized] =
    new F1Params[N, Double] {
      val params = (
        helper.shiftFromResource("sphere_func_data.txt"),
        helper.fbiasFromResource(1)
      )
    }

  implicit def f2Params[N <: Nat: ToInt: CECSized] =
    new F2Params[N, Double] {
      val params = (
        helper.shiftFromResource("schwefel_102_data.txt"),
        helper.fbiasFromResource(2)
      )
    }
  implicit def f3Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F3Params[N, Double] {
      val params = (
        helper.shiftFromResource("high_cond_elliptic_rot_data.txt"),
        helper.matrixFromResource(s"elliptic_M_D${ev.apply}.txt"),
        helper.fbiasFromResource(3)
      )
    }
  implicit def f4Params[N <: Nat: ToInt: CECSized] =
    new F4Params[N, Double] {
      val params = (
        helper.shiftFromResource("schwefel_102_data.txt"),
        helper.fbiasFromResource(4),
        Dist.stdNormal
      )
    }
  implicit def f5Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F5Params[N, Double] {
      val params = {
        val dim   = ev.apply
        val shift = helper.shiftFromResource("schwefel_206_data.txt")
        val o = shift.zipWithIndex map {
          case (oi, i) =>
            if ((i + 1) <= ceil(dim / 4.0)) -100.0
            else if ((i + 1) >= floor((3.0 * dim) / 4.0)) 100.0
            else oi
        }
        (
          o,
          helper.matrixFromResourceTail(s"schwefel_206_data.txt").t,
          helper.fbiasFromResource(5)
        )
      }
    }
  implicit def f6Params[N <: Nat: ToInt: CECSized] =
    new F6Params[N, Double] {
      val params = (
        helper.shiftFromResource("rosenbrock_func_data.txt"),
        helper.fbiasFromResource(6)
      )
    }
  implicit def f7Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F7Params[N, Double] {
      val params = (
        helper.shiftFromResource("griewank_func_data.txt"),
        helper.matrixFromResource(s"griewank_M_D${ev.apply}.txt"),
        helper.fbiasFromResource(7)
      )
    }
  implicit def f8Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F8Params[N, Double] {
      val params = {
        val o = helper.shiftFromResource("ackley_func_data.txt").zipWithIndex.map {
          case (oi, i) => if (i % 2 == 0) -32.0 else oi
        }
        (
          o,
          helper.matrixFromResource(s"ackley_M_D${ev.apply}.txt"),
          helper.fbiasFromResource(8)
        )
      }
    }
  implicit def f9Params[N <: Nat: ToInt: CECSized] =
    new F9Params[N, Double] {
      val params = (
        helper.shiftFromResource("rastrigin_func_data.txt"),
        helper.fbiasFromResource(9)
      )
    }
  implicit def f10Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F10Params[N, Double] {
      val params = (
        helper.shiftFromResource("rastrigin_func_data.txt"),
        helper.matrixFromResource(s"rastrigin_M_D${ev.apply}.txt"),
        helper.fbiasFromResource(10)
      )
    }
  implicit def f11Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F11Params[N, Double] {
      val params = (
        helper.shiftFromResource("weierstrass_data.txt"),
        helper.matrixFromResource(s"weierstrass_M_D${ev.apply}.txt"),
        helper.fbiasFromResource(11)
      )
    }
  implicit def f12Params[N <: Nat: CECSized: ToInt] =
    new F12Params[N, Double] {
      val params = (
        helper.shiftFromResourceF("schwefel_213_data.txt", _.last),
        helper.matrixFromResourceF("schwefel_213_data.txt", _.take(100)),
        helper.matrixFromResourceF("schwefel_213_data.txt", _.drop(100).take(100)),
        helper.fbiasFromResource(12)
      )
    }
  implicit def f13Params[N <: Nat: CECSized: ToInt] =
    new F13Params[N, Double] {
      val params = (
        helper.shiftFromResource("EF8F2_func_data.txt"),
        helper.fbiasFromResource(13)
      )
    }
  implicit def f14Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F14Params[N, Double] {
      val params = (
        helper.shiftFromResource("E_ScafferF6_func_data.txt"),
        helper.matrixFromResource(s"E_ScafferF6_M_D${ev.apply}.txt"),
        helper.fbiasFromResource(14)
      )
    }
  implicit def f15Params[N <: Nat: CECSized: GTEq1](implicit ev: ToInt[N]) =
    new F15Params[N, Double] {
      val params = (
        Sized.wrap(helper.shiftsFromResource("hybrid_func1_data.txt").toVector),
        Sized.wrap(Vector.fill(10)(Matrix.eye[N, Double])),
        helper.fbiasFromResource(15)
      )
    }
  implicit def f16Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F16Params[N, Double] {
      val params = (
        Sized.wrap(helper.shiftsFromResource("hybrid_func1_data.txt").toVector),
        helper.matrix10FromResource(s"hybrid_func1_M_D${ev.apply}.txt"),
        helper.fbiasFromResource(16)
      )
    }
  implicit val f17Params =
    new F17Params[Double] {
      val params = (helper.fbiasFromResource(17), Dist.stdNormal)
    }
  implicit def f18Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F18Params[N, Double] {
      val params = {
        val o: List[Dimension[N, Double]] =
          helper.shiftsFromResource("hybrid_func2_data.txt")
        val shift: Dimension10[Dimension[N, Double]] =
          Sized.wrap((o.init :+ o.last.map(_ => 0.0)).toVector)
        val m = helper.matrix10FromResource(s"hybrid_func2_M_D${ev.apply}.txt")
        (shift, m, helper.fbiasFromResource(18))
      }
    }
  implicit val f19Params =
    new F19Params[Double] {
      val params = helper.fbiasFromResource(19)
    }
  implicit def f20Params[N <: Nat: CECSized: ToInt] =
    new F20Params[N, Double] {
      val params = {
        val o: List[Dimension[N, Double]] =
          helper.shiftsFromResource("hybrid_func2_data.txt")
        val head = o.head.zipWithIndex map {
          case (oi, i) => if (i % 2 == 1) 5.0 else oi
        }
        val middle = o.tail.init
        val last   = o.last.map(_ => 0.0)
        val shift: Dimension10[Dimension[N, Double]] =
          Sized.wrap(((head :: middle) :+ last).toVector)
        (shift, helper.fbiasFromResource(20))
      }
    }
  implicit def f21Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F21Params[N, Double] {
      val params = (
        Sized.wrap(helper.shiftsFromResource("hybrid_func3_data.txt").toVector),
        helper.matrix10FromResource(s"hybrid_func3_M_D${ev.apply}.txt"),
        helper.fbiasFromResource(21)
      )
    }
  implicit def f22Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F22Params[N, Double] {
      val params = (
        Sized.wrap(helper.shiftsFromResource("hybrid_func3_data.txt").toVector),
        helper.matrix10FromResource(s"hybrid_func3_HM_D${ev.apply}.txt"),
        helper.fbiasFromResource(22)
      )
    }
  implicit val f23Params =
    new F23Params[Double] {
      val params = helper.fbiasFromResource(23)
    }
  implicit def f24Params[N <: Nat: CECSized](implicit ev: ToInt[N]) =
    new F24Params[N, Double] {
      val params = (
        Sized.wrap(helper.shiftsFromResource("hybrid_func4_data.txt").toVector),
        helper.matrix10FromResource(s"hybrid_func4_M_D${ev.apply}.txt"),
        helper.fbiasFromResource(24),
        Dist.stdNormal
      )
    }
  implicit val f25Params =
    new F25Params[Double] {
      val params = helper.fbiasFromResource(25)
    }
}
