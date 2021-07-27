package benchmarks
package cec
package cec2005


import benchmarks.matrix._

import zio.prelude.{NonEmptyList, ZValidation}
import benchmarks.Benchmarks._
import spire.algebra._
import spire.implicits._
import spire.math.{ abs, cos, round, sin, ceil, floor }

import cilib.{RVar, Dist}

/*
 * Based on: Problem Definitions and Evaluation Criteria for the CEC 2005
 * Special Session on Real-Parameter Optimization (May 2005)
 *
 * by P. N. Suganthan, N. Hansen, J. J. Liang, K. Deb, Y. -P. Chen, A. Auger,
 * S. Tiwari
 */
object Benchmarks {

  def shift[A:Field](x: NonEmptyList[A], o: NonEmptyList[Double]): NonEmptyList[A] =
    x.zipWith(o)(_ - _)

  // matrix transpose
  def transpose(matrix: Matrix[Double]) =
    Matrix.wrap(matrix.transpose: _*)

  def rotate[A:Field](x: Vector[A], m: Matrix[Double]): Vector[A] = {
    def innerProduct(other: Vector[Double])(implicit ev: Ring[A]): A =
      x.zip(other)
        .map { case (xi, oi) => xi * oi }
        .foldLeft(ev.zero)(_ + _)

    transpose(m).map(z => innerProduct(z))
  }

  /*
   * F1: Shifted Sphere Function
   * x ∈ [-100, 100]D
   */
  def f1[A: Ring: Field](x: NonEmptyList[A]): A = {
    val bias = -450
    val shifted = shift(x, Data.sphere_func_data)

    spherical(shifted) + bias
  }

  /*
   * F2: Shifted Schwefel’s Problem 1.2
   * x ∈ [-100, 100]D
   */
  def f2[A: Ring: Field](x: NonEmptyList[A]): A = {
    val bias = -450
    val shifted = shift(x, Data.schwefel_102_data)

    schwefel12(shifted) + bias
  }

  /*
   * F3: Shifted Rotated High Conditioned Elliptic Function
   * x ∈ [-100, 100]D
   */
  def f3[A: Ring: Field](x: AtLeast2List[A]): A = {
    val list = AtLeast2List.unwrap(x)
    val n = list.length
    val bias = -450

    val o = NonEmptyList.fromIterableOption(Data.high_cond_elliptic_rot_data).get

    val m =
      if (n <= 2) Data.elliptic_M_D2
      else if (n <= 10) Data.elliptic_M_D10
      else if (n <= 30) Data.elliptic_M_D30
      else Data.elliptic_M_D50

    val z = shift(list, o)
    val rotated = rotate(z.toVector, m) match {
      case x +: xs =>
        AtLeast2List.make(NonEmptyList.fromIterable(x, xs)) match {
          case ZValidation.Success(_, v) => v
          case ZValidation.Failure(_, e) => sys.error(e.toString())
        }
    }

    elliptic(rotated) + bias
  }

  /*
   * F4: Shifted Schwefel’s Problem 1.2 with Noise in Fitness
   * x ∈ [-100, 100]D
   */
  def f4[A: Field: Signed](x: NonEmptyList[A]): RVar[A] = {
    f4Noise(x, Dist.stdNormal)
  }

  def f4Noise[A: Field: Signed](x: NonEmptyList[A], noise: RVar[Double]): RVar[A] = {
    val bias = -450
    val o = Data.schwefel_102_data
    val z = shift(x, o)

    noise.map(n =>
      schwefel12(z) * (1.0 + 0.4 * abs(n)) + bias
    )
  }

  /*
   * F5: Schwefel’s Problem 2.6 with Global Optimum on Bounds
   * x ∈ [−100,100]D
   */
  def f5[A: Field: Signed: Ordering](x: NonEmptyList[A]): A = {
    val bias = -310
    val n = x.size

    val shift = Data.schwefel_206_data.head.take(n)
    val o = shift.zipWithIndex map {
      case (oi, i) =>
        if ((i + 1) <= ceil(n / 4.0)) -100.0
        else if ((i + 1) >= floor((3.0 * n) / 4.0)) 100.0
        else oi
    }

    val a = transpose(Data.schwefel_206_data.tail.take(n)) // FIXME: this is slow?

    val b = rotate(o, a)
    val z = rotate(x.toVector, a)

    (z zip b).map { case (zi, bi) => abs(zi - bi) }.max + bias
  }

  /*
   * F6: Shifted Rosenbrock’s Function
   * x ∈ [−100,100]D
   */
  def f6[A: Field](x: AtLeast2List[A]): A = {
    val bias = 390
    val o = Data.rosenbrock_func_data
    val n = shift(AtLeast2List.unwrap(x), o).map(_ + 1.0)
    val z = AtLeast2List.make(n) match {
      case ZValidation.Success(_, v) => v
      case ZValidation.Failure(_, e) => sys.error(e.toString())
    }

    rosenbrock(z) + bias
  }

  /*
   * F7: Shifted Rotated Griewank’s Function without Bounds
   */
  def f7[A: Field: NRoot: Trig](x: NonEmptyList[A]): A = {
    val bias = -180.0
    val n = x.size
    val o = NonEmptyList.fromIterable(
      Data.griewank_func_data.head,
      Data.griewank_func_data.tail.take(n-1))
    val m =
      if (n <= 2) Data.griewank_M_D2
      else if (n <= 10) Data.griewank_M_D10
      else if (n <= 30) Data.griewank_M_D30
      else Data.griewank_M_D50

    val s = shift(x, o)
    val z = rotate(s.toVector, m.take(n))

    griewank(NonEmptyList.fromIterable(z.head, z.tail)) + bias
  }

  /*
   * F8: Shifted Rotated Ackley’s Function with Global Optimum on Bounds
   * x ∈ [−32,32]D
   */
  def f8[A: Field: NRoot: Trig](x: NonEmptyList[A]): A = {
    // P.params match {
    //   case (o, m, fbias) => ackley(x.shift(o).rotate(m)) + fbias
    // }

    val bias = -140.0
    val n = x.size
    val o = NonEmptyList.fromIterable(
      Data.ackley_func_data.head,
      Data.ackley_func_data.tail.take(n-1)
    ).zipWithIndex.map {
      case (oi, i) => if (i % 2 == 0) -32.0 else oi
    }

    val m =
      if (n <= 2) Data.ackley_M_D2
      else if (n <= 10) Data.ackley_M_D10
      else if (n <= 30) Data.ackley_M_D30
      else Data.ackley_M_D50

    val s = shift(x, o)
    val z = rotate(s.toVector, m.take(n))

    ackley(NonEmptyList.fromIterableOption(z).get) + bias
  }

  /*
   * F9: Shifted Rastrigin’s Function
   * x ∈ [−5,5]D
   */
  def f9[A: Field: Trig](x: NonEmptyList[A]): A = {
    val bias = -330.0
    val o = Data.rastrigin_func_data

    rastrigin(shift(x, o)) + bias
  }

  /*
   * F10: Shifted Rotated Rastrigin’s Function
   * x ∈ [−5,5]D
   */
  def f10[A: Field: Trig](x: NonEmptyList[A]): A = {
    // P.params match {
    //   case (o, m, fbias) => rastrigin(x.shift(o).rotate(m)) + fbias
    // }
    val n = x.size
    val bias = -330.0
    val o = Data.rastrigin_func_data

    val m =
      if (n <= 2) Data.rastrigin_M_D2
      else if (n <= 10) Data.rastrigin_M_D10
      else if (n <= 30) Data.rastrigin_M_D30
      else Data.rastrigin_M_D50

    val z = rotate(shift(x, o).toVector, m.take(n))

    rastrigin(NonEmptyList.fromIterable(z.head, z.tail)) + bias
  }

  /*
   * F11: Shifted Rotated Weierstrass Function
   * x ∈ [−0.5,0.5]D
   */
  def f11[A: Field: Trig](x: NonEmptyList[A]): A = {
    // P.params match {
    //   case (o, m, fbias) => weierstrass(x.shift(o).rotate(m)) + fbias
    val n = x.size
    val bias = 90.0
    val o = Data.weierstrass_data

    val m =
      if (n <= 2) Data.weierstrass_M_D2
      else if (n <= 10) Data.weierstrass_M_D10
      else if (n <= 30) Data.weierstrass_M_D30
      else Data.weierstrass_M_D50

    val z = rotate(shift(x, o).toVector, m.take(n))

    weierstrass(NonEmptyList.fromIterable(z.head, z.tail)) + bias
  }

  /*
   * F12: Schwefel’s Problem 2.13
   * x ∈ [−π,π]D
   *
   * Note: the algorithm has been modified to avoid col/row indexing.
   * 'a' and 'b' must be row-major matrices.
   */
  def f12[A: Field: Trig](x: NonEmptyList[A]): A = {
    val n = x.size
    val bias = -460.0
    val alpha = Data.schwefel_213_data.last.take(n)
    val a = Data.schwefel_213_data.take(100).map(_.take(n))
    val b = Data.schwefel_213_data.drop(100).take(100).map(_.take(n))

    println(s"sizes: n: ${n} alpha ${alpha.size}, a: ${a.head.size}, b: ${b.head.size}")

    val A = a.zip(b).map {
      case (ac, bc) =>
        mapSum(alpha.zip(ac).zip(bc)) {
          case ((ai, aci), bci) =>
            aci * sin(ai) + bci * cos(ai)
        }
    }

    val B = a.zip(b).map {
      case (ac, bc) =>
        mapSum(x.toVector zip ac zip bc)  {
          case ((xi, aci), bci) =>
            aci * sin(xi) + bci * cos(xi)
        }
    }

    val result = mapSum(A zip B) { case (axi, bxi) => (axi - bxi) ** 2 }

    result + bias
  }

  // val fbias =
  //   NonEmptyList(-130, -300, 120, 120, 120, 10, 10, 10, 360, 360, 360, 260, 260)

  /*
   * F13: Shifted Expanded Griewank’s plus Rosenbrock’s Function (F8F2)
   * x ∈ [−5,5]D
   */
  def f13[A: Field: NRoot: Trig](x: AtLeast2List[A]): A = {
    val bias = -130.0
    val o = Data.EF8F2_func_data

    // P.params match {
    //   case (o, fbias) => {
    val z  = shift(AtLeast2List.unwrap(x), o).map { _ + 1.0 }
    val ps = pairs(z.toList :+ z.head).map { case (a, b) =>
      AtLeast2List.make(NonEmptyList(a, b)) match {
        case ZValidation.Failure(_, e) => sys.error(e.toString())
        case ZValidation.Success(_, a) => a
      }
    }

    val result = mapSum(ps) { pair =>
      griewank(NonEmptyList(rosenbrock(pair)))
    }

    result + bias
  }

  /*
   * F14 Shifted Rotated Expanded Scaffer’s F6 Function
   * x ∈ [−100,100]D
   */
  def f14[A: Field: NRoot: Trig](x: AtLeast2List[A]): A = {
    // P.params match {
    //   case (o, m, fbias) =>
    //     val z = x.shift(o).rotate(m)
    //     (z.toList :+ z.head).pairs.mapSum { case (a, b) => schaffer6(Sized(a, b)) } + fbias
    // }
    val bias = -300.0
    val n = AtLeast2List.unwrap(x).size
    val o = Data.scafferF6_func_data

    val m =
      if (n <= 2) Data.scafferF6_M_D2
      else if (n <= 10) Data.scafferF6_M_D10
      else if (n <= 30) Data.scafferF6_M_D30
      else Data.scafferF6_M_D50

    val z = rotate(shift(AtLeast2List.unwrap(x), o).toVector, m)

    val result = mapSum(pairs(z.toList :+ z.head)) { case (a, b) =>
      val list = AtLeast2List.make(NonEmptyList(a, b)) match {
        case ZValidation.Failure(_, e) => sys.error(e.toString())
        case ZValidation.Success(_, a) => a
      }

      schaffer6(list)
    }

    result + bias
  }

  /*
   * F15 Hybrid Composition Function
   * x ∈ [−5,5]D
   */
  // def f15[N <: Nat: ToInt, A: Field: Trig: NRoot: Signed: Ordering](implicit P: F15Params[N, A]): Dimension[N, A] => A =
  //   P.params match {
  //     case (o, m, fbias) => {
  //       val funcs = Sized(
  //         rastrigin[N, A] _,
  //         rastrigin[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         griewank[N, A] _,
  //         griewank[N, A] _,
  //         ackley[N, A] _,
  //         ackley[N, A] _,
  //         spherical[N, A] _,
  //         spherical[N, A] _
  //       )
  //       val b = Sized(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
  //       val λ = Sized(1.0, 1.0, 10.0, 10.0, 5.0 / 60.0, 5.0 / 60.0, 5.0 / 32.0, 5.0 / 32.0, 5.0 / 100.0, 5.0 / 100.0)
  //       val σ = Sized(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  //       val h = Helper.hybrid[nat._10, N, A](b, o, m, funcs, λ, σ)
  //       x => h(x) + fbias
  //     }
  //   }

  /*
   * F16: Rotated Version of Hybrid Composition Function F15
   * x ∈ [−5,5]D
   */
  // def f16[N <: Nat: ToInt, A: Field: NRoot: Signed: Ordering: Trig](implicit P: F16Params[N, A]): Dimension[N, A] => A =
  //   P.params match {
  //     case (o, m, fbias) => {
  //       val funcs = Sized(
  //         rastrigin[N, A] _,
  //         rastrigin[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         griewank[N, A] _,
  //         griewank[N, A] _,
  //         ackley[N, A] _,
  //         ackley[N, A] _,
  //         spherical[N, A] _,
  //         spherical[N, A] _
  //       )
  //       val b = Sized(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
  //       val λ = Sized(1.0, 1.0, 10.0, 10.0, 5.0 / 60.0, 5.0 / 60.0, 5.0 / 32.0, 5.0 / 32.0, 5.0 / 100.0, 5.0 / 100.0)
  //       val σ = Sized(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  //       val h = Helper.hybrid[nat._10, N, A](b, o, m, funcs, λ, σ)

  //       x => h(x) + fbias
  //     }
  //   }

  /*
   * F17: F16 with Noise in Fitness
   * x ∈ [−5,5]D
   */
  // def f17[N <: Nat: ToInt, A: Field: NRoot: Signed: Ordering: Trig](
  //   x: Dimension[N, A]
  // )(implicit P16: F16Params[N, A], P17: F17Params[A]): RVar[A] =
  //   (P16.params, P17.params) match {
  //     case ((_, _, fbias16), (fbias, noise)) =>
  //       noise map { n =>
  //         val f  = f16[N, A]
  //         val gx = f(x) - fbias16
  //         gx * (1.0 + 0.2 * abs(n)) + fbias
  //       }
  //   }

  /*
   * F18: Rotated Hybrid Composition Function
   * x ∈ [−5,5]D
   */
  // def f18[N <: Nat: ToInt, A: Field: NRoot: Signed: Ordering: Trig](implicit P: F18Params[N, A]): Dimension[N, A] => A =
  //   P.params match {
  //     case (o, m, fbias) => {
  //       val funcs = Sized(
  //         ackley[N, A] _,
  //         ackley[N, A] _,
  //         rastrigin[N, A] _,
  //         rastrigin[N, A] _,
  //         spherical[N, A] _,
  //         spherical[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         griewank[N, A] _,
  //         griewank[N, A] _
  //       )
  //       val b = Sized(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
  //       val λ = Sized(
  //         2 * 5.0 / 32.0,
  //         5.0 / 32.0,
  //         2.0 * 1.0,
  //         1.0,
  //         2 * 5.0 / 100.0,
  //         5.0 / 100.0,
  //         2.0 * 10.0,
  //         10.0,
  //         2.0 * 5.0 / 60.0,
  //         5.0 / 60.0
  //       )
  //       val σ = Sized(1.0, 2.0, 1.5, 1.5, 1.0, 1.0, 1.5, 1.5, 2.0, 2.0)
  //       val h = Helper.hybrid[nat._10, N, A](b, o, m, funcs, λ, σ)

  //       x => h(x) + fbias
  //     }
  //   }

  /*
   * F19: Rotated Hybrid Composition Function with narrow basin global optimum
   * x ∈ [−5,5]D
   */
  // def f19[N <: Nat: ToInt, A: Field: NRoot: Trig: Signed: Ordering](
  //   implicit P18: F18Params[N, A],
  //   P19: F19Params[A]
  // ): Dimension[N, A] => A =
  //   (P18.params, P19.params) match {
  //     case ((o, m, _), fbias) =>
  //       val funcs = Sized(
  //         ackley[N, A] _,
  //         ackley[N, A] _,
  //         rastrigin[N, A] _,
  //         rastrigin[N, A] _,
  //         spherical[N, A] _,
  //         spherical[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         griewank[N, A] _,
  //         griewank[N, A] _
  //       )
  //       val b = Sized(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
  //       val λ = Sized(
  //         0.1 * 5.0 / 32.0,
  //         5.0 / 32.0,
  //         2.0 * 1.0,
  //         1.0,
  //         2.0 * 5.0 / 100.0,
  //         5.0 / 100.0,
  //         2.0 * 10.0,
  //         10.0,
  //         2.0 * 5.0 / 60.0,
  //         5.0 / 60.0
  //       )
  //       val σ = Sized(0.1, 2.0, 1.5, 1.5, 1.0, 1.0, 1.5, 1.5, 2.0, 2.0)
  //       val h = Helper.hybrid[nat._10, N, A](b, o, m, funcs, λ, σ)

  //       x => h(x) + fbias
  //   }

  /*
   * F20: Rotated Hybrid Composition Function with Global Optimum on the Bounds
   * x ∈ [−5,5]D
   */
  // def f20[N <: Nat: ToInt, A: Field: NRoot: Ordering: Signed: Trig](
  //   implicit P18: F18Params[N, A],
  //   P20: F20Params[N, A]
  // ): Dimension[N, A] => A =
  //   (P18.params, P20.params) match {
  //     case ((_, m, _), (o, fbias)) =>
  //       val funcs = Sized(
  //         ackley[N, A] _,
  //         ackley[N, A] _,
  //         rastrigin[N, A] _,
  //         rastrigin[N, A] _,
  //         spherical[N, A] _,
  //         spherical[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         griewank[N, A] _,
  //         griewank[N, A] _
  //       )
  //       val b = Sized(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
  //       val λ = Sized(
  //         2 * 5.0 / 32.0,
  //         5.0 / 32.0,
  //         2.0 * 1.0,
  //         1.0,
  //         2 * 5.0 / 100.0,
  //         5.0 / 100.0,
  //         2.0 * 10.0,
  //         10.0,
  //         2.0 * 5.0 / 60.0,
  //         5.0 / 60.0
  //       )
  //       val σ = Sized(1.0, 2.0, 1.5, 1.5, 1.0, 1.0, 1.5, 1.5, 2.0, 2.0)
  //       val h = Helper.hybrid[nat._10, N, A](b, o, m, funcs, λ, σ)

  //       x => h(x) + fbias
  //   }

  // private def expandedShafferF6[N <: Nat: GTEq2: HasHead, A: Field: NRoot: Trig](x: Dimension[N, A]): A =
  //   (x.toList :+ x.head).pairs mapSum {
  //     case (a, b) => schaffer6(Sized(a, b))
  //   }

  // def f8f2[N <: Nat: GTEq2: HasHead, A: Field: NRoot: Trig](x: Dimension[N, A]): A =
  //   (x.toList :+ x.head).pairs mapSum {
  //     case (a, b) => {
  //       griewank(Sized(rosenbrock(Sized(a, b))))
  //     }
  //   }

  /*
   * F21: Rotated Hybrid Composition Function
   * x ∈ [−5,5]D
   */
  // def f21[N <: Nat: ToInt: GTEq2: HasHead, A: Field: Ordering: NRoot: Signed: Trig](
  //   implicit P: F21Params[N, A]
  // ): Dimension[N, A] => A =
  //   P.params match {
  //     case (o, m, fbias) => {
  //       val funcs = Sized(
  //         expandedShafferF6[N, A] _,
  //         expandedShafferF6[N, A] _,
  //         rastrigin[N, A] _,
  //         rastrigin[N, A] _,
  //         f8f2[N, A] _,
  //         f8f2[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         griewank[N, A] _,
  //         griewank[N, A] _
  //       )
  //       val b = Sized(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
  //       val λ = Sized(
  //         5.0 * 5.0 / 100.0,
  //         5.0 / 100.0,
  //         5.0 * 1.0,
  //         1.0,
  //         5.0 * 1.0,
  //         1.0,
  //         5.0 * 10.0,
  //         10.0,
  //         5.0 * 5.0 / 200.0,
  //         5.0 / 200.0
  //       )
  //       val σ = Sized(1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0)
  //       val h = Helper.hybrid[nat._10, N, A](b, o, m, funcs, λ, σ)

  //       x => h(x) + fbias
  //     }
  //   }

  /*
   * F22: Rotated Hybrid Composition Function with High Condition Number Matrix
   * x ∈ [−5,5]D
   */
  // def f22[N <: Nat: ToInt: GTEq2: HasHead, A: Field: NRoot: Ordering: Signed: Trig](
  //   implicit P: F22Params[N, A]
  // ): Dimension[N, A] => A =
  //   P.params match {
  //     case (o, m, fbias) => {
  //       val funcs = Sized(
  //         expandedShafferF6[N, A] _,
  //         expandedShafferF6[N, A] _,
  //         rastrigin[N, A] _,
  //         rastrigin[N, A] _,
  //         f8f2[N, A] _,
  //         f8f2[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         griewank[N, A] _,
  //         griewank[N, A] _
  //       )
  //       val b = Sized(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
  //       val λ = Sized(
  //         5.0 * 5.0 / 100.0,
  //         5.0 / 100.0,
  //         5.0 * 1.0,
  //         1.0,
  //         5.0 * 1.0,
  //         1.0,
  //         5.0 * 10.0,
  //         10.0,
  //         5.0 * 5.0 / 200.0,
  //         5.0 / 200.0
  //       )
  //       val σ = Sized(1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0)
  //       val h = Helper.hybrid[nat._10, N, A](b, o, m, funcs, λ, σ)

  //       x => h(x) + fbias
  //     }
  //   }

  /*
   * F23: Non-Continuous Rotated Hybrid Composition Function
   * x ∈ [−5,5]D
   */
  // def f23[N <: Nat: ToInt: GTEq2: HasHead, A: IsReal: Signed: Ordering: NRoot: Trig](
  //   implicit P21: F21Params[N, A],
  //   P23: F23Params[A],
  //   A: Field[A]
  // ): Dimension[N, A] => A =
  //   (P21.params, P23.params) match {
  //     case ((o, _, f21bias), fbias) =>
  //       x =>
  //         val xModified = (x zip o.head) map {
  //           case (xj, o1j) =>
  //             if (abs(xj - o1j) < A.fromDouble(0.5)) xj
  //             else round(2.0 * xj) / 2.0
  //         }
  //         val f = f21[N, A]
  //         f(xModified) - f21bias + fbias
  //   }

  /*
   * F24: Rotated Hybrid Composition Function
   * x ∈ [−5,5]D
   */
  // def f24[N <: Nat: ToInt: GTEq2: HasHead, A: Field: IsReal: NRoot: Trig: Signed: Ordering](
  //   implicit P: F24Params[N, A]
  // ): Dimension[N, A] => RVar[A] =
  //   P.params match {
  //     case (o, m, fbias, noise) =>
  //       def preRound(x: Dimension[N, A]) =
  //         x map { xj =>
  //           if (abs(xj) < implicitly[Field[A]].fromDouble(0.5)) xj
  //           else round(2.0 * xj) / 2.0
  //         }
  //       val funcsSeq: Vector[Dimension[N, A] => RVar[A]] =
  //         Vector(
  //           x => RVar.point(weierstrass[N, A](x)),
  //           x => RVar.point(expandedShafferF6[N, A](x)),
  //           x => RVar.point(f8f2[N, A](x)),
  //           x => RVar.point(ackley[N, A](x)),
  //           x => RVar.point(rastrigin[N, A](x)),
  //           x => RVar.point(griewank[N, A](x)),
  //           x => RVar.point(expandedShafferF6[N, A](preRound(x))),
  //           x => RVar.point(rastrigin[N, A](preRound(x))),
  //           x => RVar.point(elliptic(x)),
  //           x =>
  //             noise.map { n =>
  //               spherical(x) * (1.0 + 0.1 * n)
  //             }
  //         )
  //       val funcs: Dimension10[Dimension[N, A] => RVar[A]] = Sized.wrap(funcsSeq)
  //       val b                                              = Sized(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
  //       val λ                                              = Sized(10.0, 5.0 / 20.0, 1.0, 5.0 / 32.0, 1.0, 5.0 / 100.0, 5.0 / 50.0, 1.0, 5.0 / 100.0, 5.0 / 100.0)
  //       val σ                                              = Sized(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0)
  //       val h                                              = Helper.hybridR[nat._10, N, A](b, o, m, funcs, λ, σ)
  //       x => h(x) map { _ + fbias }
  //   }

  /*
   * F25: Rotated Hybrid Composition Function without bounds
   * x ∈ [2, 5]D
   */
  // def f25[N <: Nat: ToInt: GTEq2: HasHead, A: Field: IsReal: Ordering: NRoot: Signed: Trig](
  //   implicit P24: F24Params[N, A],
  //   P25: F25Params[A]
  // ): Dimension[N, A] => RVar[A] =
  //   (P24.params, P25.params) match {
  //     case ((_, _, f24bias, _), fbias) =>
  //       val f = f24[N, A]
  //       x => f(x) map { _ - f24bias + fbias }
  //   }
}
