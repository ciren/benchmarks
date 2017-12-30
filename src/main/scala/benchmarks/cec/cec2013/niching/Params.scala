package benchmarks
package cec
package cec2013
package niching

import shapeless._
import shapeless.ops.nat.ToInt

import benchmarks.dimension._
import benchmarks.matrix._
import benchmarks.cec.Helper

trait F8Params  [N<:Nat]   { val params: Dimension[N,Int] }
trait F9Params  [N<:Nat,A] { val params: (Dimension6[Dimension[N,A]], A) }
trait F10Params [N<:Nat,A] { val params: (Dimension8[Dimension[N,A]], A) }
trait F11Params [N<:Nat,A] {
  val params: (Dimension6[Dimension[N,A]], Dimension6[Matrix[N,N,A]], A)
}
trait F12Params [N<:Nat,A] {
  val params: (Dimension8[Dimension[N,A]], Dimension8[Matrix[N,N,A]], A)
}

sealed trait CEC2013Sized[N<:Nat]

trait Params {
  implicit object cec2013Sized2  extends CEC2013Sized[nat._2]
  implicit object cec2013Sized3  extends CEC2013Sized[nat._3]
  implicit object cec2013Sized5  extends CEC2013Sized[nat._5]
  implicit object cec2013Sized10 extends CEC2013Sized[nat._10]
  implicit object cec2013Sized20 extends CEC2013Sized[nat._20]

  implicit val f8Params2 = new F8Params[nat._2] {
    val params = Sized(3, 4)
  }
  implicit val f8Params16 = new F8Params[nat._16] {
    val params = Sized(1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 3, 1, 1, 1, 4)
  }

  implicit def cec2013NichingF9Params[N<:Nat:ToInt:CEC2013Sized] = new F9Params[N,Double] {
    val helper = Helper("cec2013/niching")
    val dim = implicitly[ToInt[N]].apply
    val params = (
      Sized.wrap(helper.shiftsFromResource[N](s"CF1_M_D${dim}_opt.dat").toVector),
      -2500.0
    )
  }

  implicit def cec2013NichingF10Params[N<:Nat:ToInt:CEC2013Sized] = new F10Params[N,Double] {
    val helper = Helper("cec2013/niching")
    val dim = implicitly[ToInt[N]].apply
    val params = (
      Sized.wrap(helper.shiftsFromResource[N](s"CF2_M_D${dim}_opt.dat").toVector),
      -2500.0
    )
  }

  implicit def cec2013NichingF11Params[N<:Nat:GTEq1:CEC2013Sized:ToInt] = new F11Params[N,Double] {
    val helper = Helper("cec2013/niching")
    val dim = implicitly[ToInt[N]].apply
    val params = (
      Sized.wrap(helper.shiftsFromResource[N](s"CF3_M_D${dim}_opt.dat").toVector),
      helper.matricesFromResource[nat._6,N](s"CF3_M_D${dim}.dat"),
      -4000.0
    )
  }

  implicit def cec2013NichingF12Params[N<:Nat:GTEq1:CEC2013Sized:ToInt] = new F12Params[N,Double] {
    val helper = Helper("cec2013/niching")
    val dim = implicitly[ToInt[N]].apply
    val params = (
      Sized.wrap(helper.shiftsFromResource[N](s"CF4_M_D${dim}_opt.dat").toVector),
      helper.matricesFromResource[nat._8,N](s"CF4_M_D${dim}.dat"),
      -4000.0
    )
  }

}
