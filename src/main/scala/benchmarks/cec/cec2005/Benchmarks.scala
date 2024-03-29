package benchmarks
package cec
package cec2005


import benchmarks.matrix._

import zio.prelude.{NonEmptyList, ZValidation}
import benchmarks.Benchmarks._
import scala.math._

import cilib.{RVar, Dist}
import cilib.NonEmptyVector

/*
 * Based on: Problem Definitions and Evaluation Criteria for the CEC 2005
 * Special Session on Real-Parameter Optimization (May 2005)
 *
 * by P. N. Suganthan, N. Hansen, J. J. Liang, K. Deb, Y. -P. Chen, A. Auger,
 * S. Tiwari
 */
object Benchmarks {

  def shift(x: NonEmptyVector[Double], o: NonEmptyList[Double]): NonEmptyList[Double] =
    NonEmptyList.fromIterableOption(x.toChunk).get.zipWith(o)(_ - _)

  def rotate(x: Vector[Double], m: Matrix[Double]): Vector[Double] = {
    def innerProduct(other: Vector[Double]): Double =
      x.zip(other)
        .map { case (xi, oi) => xi * oi }
        .foldLeft(0.0)(_ + _)

    m.transpose.toVector.map(z => innerProduct(z))
  }

  /*
   * F1: Shifted Sphere Function
   * x ∈ [-100, 100]D
   */
  def f1(x: NonEmptyVector[Double]): Double = {
    val bias = -450
    val shifted = shift(x, Data.sphere_func_data)

    spherical(shifted) + bias
  }

  /*
   * F2: Shifted Schwefel’s Problem 1.2
   * x ∈ [-100, 100]D
   */
  def f2(x: NonEmptyVector[Double]): Double = {
    val bias = -450
    val shifted = shift(x, Data.schwefel_102_data)

    schwefel12(shifted) + bias
  }

  /*
   * F3: Shifted Rotated High Conditioned Elliptic Function
   * x ∈ [-100, 100]D
   */
  def f3(x: AtLeast2List): Double = {
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
        AtLeast2List.make(NonEmptyVector.fromIterable(x, xs)) match {
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
  def f4(x: NonEmptyVector[Double]): RVar[Double] = {
    f4Noise(x, Dist.stdNormal)
  }

  def f4Noise(x: NonEmptyVector[Double], noise: RVar[Double]): RVar[Double] = {
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
  def f5(x: NonEmptyVector[Double]): Double = {
    val bias = -310
    val n = x.length

    val data = Data.schwefel_206_data.limit(n)
    val shift = data.shift

    val o = shift.zipWithIndex map {
      case (oi, i) =>
        if ((i + 1) <= ceil(n / 4.0)) -100.0
        else if ((i + 1) >= floor((3.0 * n) / 4.0)) 100.0
        else oi
    }

    val a = data.matrixA.transpose.mapRow(_.take(n))

    val b = rotate(o, a)
    val z = rotate(x.toChunk.toVector, a)

    (z zip b).map { case (zi, bi) => abs(zi - bi) }.max + bias
  }

  /*
   * F6: Shifted Rosenbrock’s Function
   * x ∈ [−100,100]D
   */
  def f6(x: AtLeast2List): Double = {
    val bias = 390
    val o = Data.rosenbrock_func_data
    val nel = AtLeast2List.unwrap(x)
    val n = NonEmptyVector.fromIterableOption(shift(nel, o).map(_ + 1.0)).get
    val z = AtLeast2List.make(n) match {
      case ZValidation.Success(_, v) => v
      case ZValidation.Failure(_, e) => sys.error(e.toString())
    }

    rosenbrock(z) + bias
  }

  /*
   * F7: Shifted Rotated Griewank’s Function without Bounds
   */
  def f7(x: NonEmptyVector[Double]): Double = {
    val bias = -180.0
    val n = x.length
    val o = NonEmptyList.fromIterable(
      Data.griewank_func_data.head,
      Data.griewank_func_data.tail.take(n-1))
    val m =
      if (n <= 2) Data.griewank_M_D2
      else if (n <= 10) Data.griewank_M_D10
      else if (n <= 30) Data.griewank_M_D30
      else Data.griewank_M_D50

    val s = shift(x, o)
    val z = rotate(s.toVector, m.mapRow(_.take(n)))

    griewank(NonEmptyList.fromIterable(z.head, z.tail)) + bias
  }

  /*
   * F8: Shifted Rotated Ackley’s Function with Global Optimum on Bounds
   * x ∈ [−32,32]D
   */
  def f8(x: NonEmptyVector[Double]): Double = {
    // P.params match {
    //   case (o, m, fbias) => ackley(x.shift(o).rotate(m)) + fbias
    // }

    val bias = -140.0
    val n = x.length
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
    val z = rotate(s.toVector, m.mapRow(_.take(n)))

    ackley(NonEmptyList.fromIterableOption(z).get) + bias
  }

  /*
   * F9: Shifted Rastrigin’s Function
   * x ∈ [−5,5]D
   */
  def f9(x: NonEmptyVector[Double]): Double = {
    val bias = -330.0
    val o = NonEmptyList.fromIterable(
      Data.rastrigin_func_data.head,
      Data.rastrigin_func_data.tail.take(x.length-1)
    )

    rastrigin(shift(x, o)) + bias
  }

  /*
   * F10: Shifted Rotated Rastrigin’s Function
   * x ∈ [−5,5]D
   */
  def f10(x: NonEmptyVector[Double]): Double = {
    // P.params match {
    //   case (o, m, fbias) => rastrigin(x.shift(o).rotate(m)) + fbias
    // }
    val n = x.length
    val bias = -330.0
    val o = Data.rastrigin_func_data

    val m =
      if (n <= 2) Data.rastrigin_M_D2
      else if (n <= 10) Data.rastrigin_M_D10
      else if (n <= 30) Data.rastrigin_M_D30
      else Data.rastrigin_M_D50

    val z = rotate(shift(x, o).toVector, m.mapRow(_.take(n)))

    rastrigin(NonEmptyList.fromIterable(z.head, z.tail)) + bias
  }

  /*
   * F11: Shifted Rotated Weierstrass Function
   * x ∈ [−0.5,0.5]D
   */
  def f11(x: NonEmptyVector[Double]): Double = {
    // P.params match {
    //   case (o, m, fbias) => weierstrass(x.shift(o).rotate(m)) + fbias
    val n = x.length
    val bias = 90.0
    val o = Data.weierstrass_data

    val m =
      if (n <= 2) Data.weierstrass_M_D2
      else if (n <= 10) Data.weierstrass_M_D10
      else if (n <= 30) Data.weierstrass_M_D30
      else Data.weierstrass_M_D50

    val z = rotate(shift(x, o).toVector, m.mapRow(_.take(n)))

    weierstrass(NonEmptyList.fromIterable(z.head, z.tail)) + bias
  }

  /*
   * F12: Schwefel’s Problem 2.13
   * x ∈ [−π,π]D
   *
   * Note: the algorithm has been modified to avoid col/row indexing.
   * 'a' and 'b' must be row-major matrices.
   */
  def f12(x: NonEmptyVector[Double]): Double = {
    val n = x.length
    val bias = -460.0

    val data = Data.schwefel_213_data.limit(n)
    val a = data.matrixA.toVector
    val b = data.matrixB.toVector
    val alpha = data.alpha

    val A = a.zip(b).map {
      case (ac, bc) =>
        mapSum(alpha.zip(ac).zip(bc)) {
          case ((ai, aci), bci) =>
            aci * sin(ai) + bci * cos(ai)
        }
    }

    val B = a.zip(b).map {
      case (ac, bc) =>
        mapSum(x.toChunk.toVector zip ac zip bc)  {
          case ((xi, aci), bci) =>
            aci * sin(xi) + bci * cos(xi)
        }
    }

    val result = mapSum(A zip B) { case (axi, bxi) => (axi - bxi) * (axi - bxi) }

    result + bias
  }

  /*
   * F13: Shifted Expanded Griewank’s plus Rosenbrock’s Function (F8F2)
   * x ∈ [−5,5]D
   */
  def f13(x: AtLeast2List): Double = {
    val bias = -130.0
    val o = Data.EF8F2_func_data

    // P.params match {
    //   case (o, fbias) => {
    val z  = shift(AtLeast2List.unwrap(x), o).map { _ + 1.0 }
    val ps = pairs(z.toList :+ z.head).map { case (a, b) =>
      AtLeast2List.make(NonEmptyVector(a, b)) match {
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
  def f14(x: AtLeast2List): Double = {
    // P.params match {
    //   case (o, m, fbias) =>
    //     val z = x.shift(o).rotate(m)
    //     (z.toList :+ z.head).pairs.mapSum { case (a, b) => schaffer6(Sized(a, b)) } + fbias
    // }
    val bias = -300.0
    val n = AtLeast2List.unwrap(x).length
    val o = Data.scafferF6_func_data

    val m =
      if (n <= 2) Data.scafferF6_M_D2
      else if (n <= 10) Data.scafferF6_M_D10
      else if (n <= 30) Data.scafferF6_M_D30
      else Data.scafferF6_M_D50

    val z = rotate(shift(AtLeast2List.unwrap(x), o).toVector, m)

    val result = mapSum(pairs(z.toList :+ z.head)) { case (a, b) =>
      val list = AtLeast2List.make(NonEmptyVector(a, b)) match {
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
  def f15(x: NonEmptyVector[Double]): Double = {
    // P.params match {
    //   case (o, m, fbias) => {
    val fbias = 120.0

    val n = x.length
    val o = Data.hybrid_func1_data.shiftVectors
    val m =
      NonEmptyVector.fromIterable(
        Matrix.identity[Double](n),
        List.fill(9)(Matrix.identity[Double](n - 1))
      )

    val funcs = NonEmptyVector(
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => ackley(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => ackley(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => spherical(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => spherical(NonEmptyList.fromCons(a.toCons))
    )
    val b = NonEmptyVector(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
    val λ = NonEmptyVector(1.0, 1.0, 10.0, 10.0, 5.0 / 60.0, 5.0 / 60.0, 5.0 / 32.0, 5.0 / 32.0, 5.0 / 100.0, 5.0 / 100.0)
    val σ = NonEmptyVector(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
    val h = Helper.hybrid(b, o, m, funcs, λ, σ)

    h(x) + fbias
  }


  // val fbias =
  //   NonEmptyVector(-300, 120, 120, 120, 10, 10, 10, 360, 360, 360, 260, 260)


  /*
   * F16: Rotated Version of Hybrid Composition Function F15
   * x ∈ [−5,5]D
   */
  def f16(x: NonEmptyVector[Double]): Double = {
    // P.params match {
    //   case (o, m, fbias) => {
    val n = x.length
    val fbias = 120.0

    val o = Data.hybrid_func1_data.limit(n).shiftVectors //.map(z => NonEmptyVector.fromChunk(z.toChunk.take(n)).get)
    val m: NonEmptyVector[Matrix[Double]] = {
      val matrixList =
        if (n <= 2) Data.hybrid_func1_M_D2
        else if (n <= 10) Data.hybrid_func1_M_D10
        else if (n <= 30) Data.hybrid_func1_M_D30
        else Data.hybrid_func1_M_D50

      matrixList.map(_.mapRow(_.take(n)))
    }

    val funcs = NonEmptyVector(
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => ackley(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => ackley(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => spherical(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => spherical(NonEmptyList.fromCons(a.toCons))
    )
    val b = NonEmptyVector(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
    val λ = NonEmptyVector(1.0, 1.0, 10.0, 10.0, 5.0 / 60.0, 5.0 / 60.0, 5.0 / 32.0, 5.0 / 32.0, 5.0 / 100.0, 5.0 / 100.0)
    val σ = NonEmptyVector(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
    val h = Helper.hybrid(b, o, m, funcs, λ, σ)

    h(x) + fbias
    //   }
    // }
  }

  /*
   * F17: F16 with Noise in Fitness
   * x ∈ [−5,5]D
   */
  def f17(x: NonEmptyVector[Double]): RVar[Double] = {
   // (P16.params, P17.params) match {
   //      case ((_, _, fbias16), (fbias, noise)) =>
    val noise = Dist.stdNormal
    val fbias16 = 120.0
    val fbias = 120.0

    noise map { n =>
      val gx = f16(x) - fbias16
      gx * (1.0 + 0.2 * abs(n)) + fbias
    }
    //}
  }

  /*
   * F18: Rotated Hybrid Composition Function
   * x ∈ [−5,5]D
   */
  def f18(x: NonEmptyVector[Double]): Double = {
    // P.params match {
    //   case (o, m, fbias) => {
    val n = x.length
    val fbias = 10.0
    val o = Data.hybrid_func2_data.limit(n).shiftVectors
    val m: NonEmptyVector[Matrix[Double]] = {
      val matrixList =
        if (n <= 2) Data.hybrid_func2_M_D2
        else if (n <= 10) Data.hybrid_func2_M_D10
        else if (n <= 30) Data.hybrid_func2_M_D30
        else Data.hybrid_func2_M_D50

      matrixList.map(_.mapRow(_.take(n)))
    }

    val funcs = NonEmptyVector(
      (a: NonEmptyVector[Double]) => ackley(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => ackley(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => spherical(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => spherical(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons))
    )
    val b = NonEmptyVector(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
    val λ = NonEmptyVector(
      2 * 5.0 / 32.0,
      5.0 / 32.0,
      2.0 * 1.0,
      1.0,
      2 * 5.0 / 100.0,
      5.0 / 100.0,
      2.0 * 10.0,
      10.0,
      2.0 * 5.0 / 60.0,
      5.0 / 60.0
    )
    val σ = NonEmptyVector(1.0, 2.0, 1.5, 1.5, 1.0, 1.0, 1.5, 1.5, 2.0, 2.0)
    val h = Helper.hybrid(b, o, m, funcs, λ, σ)

    h(x) + fbias
    //   }
    // }
  }

  /*
   * F19: Rotated Hybrid Composition Function with narrow basin global optimum
   * x ∈ [−5,5]D
   */
  def f19(x: NonEmptyVector[Double]): Double = {
    // (P18.params, P19.params) match {
    //   case ((o, m, _), fbias) =>
    val n = x.length
    val fbias = 10.0

    val o = Data.hybrid_func2_data.limit(n).shiftVectors
    val m: NonEmptyVector[Matrix[Double]] = {
      val matrixList =
        if (n <= 2) Data.hybrid_func2_M_D2
        else if (n <= 10) Data.hybrid_func2_M_D10
        else if (n <= 30) Data.hybrid_func2_M_D30
        else Data.hybrid_func2_M_D50

      matrixList.map(_.mapRow(_.take(n)))
    }

    val funcs = NonEmptyVector(
      (a: NonEmptyVector[Double]) => ackley(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => ackley(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => spherical(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => spherical(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons))
    )
    val b = NonEmptyVector(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
    val λ = NonEmptyVector(
      0.1 * 5.0 / 32.0,
      5.0 / 32.0,
      2.0 * 1.0,
      1.0,
      2.0 * 5.0 / 100.0,
      5.0 / 100.0,
      2.0 * 10.0,
      10.0,
      2.0 * 5.0 / 60.0,
      5.0 / 60.0
    )
    val σ = NonEmptyVector(0.1, 2.0, 1.5, 1.5, 1.0, 1.0, 1.5, 1.5, 2.0, 2.0)
    val h = Helper.hybrid(b, o, m, funcs, λ, σ)

    h(x) + fbias
  }

  /*
   * F20: Rotated Hybrid Composition Function with Global Optimum on the Bounds
   * x ∈ [−5,5]D
   */
  def f20(x: NonEmptyVector[Double]): Double = {
    // (P18.params, P20.params) match {
    //   case ((_, m, _), (o, fbias)) =>
    val n = x.length
    val fbias = 10.0

    val o = {
      val shifts = Data.hybrid_func2_data.limit(n).shiftVectors
      val head: zio.Chunk[Double] = shifts.head.zipWithIndex.map {
        case (oi, i) => if (i % 2 == 1) 5.0 else oi
      }.toChunk
      val middle: zio.Chunk[NonEmptyVector[Double]] = shifts.toChunk.drop(1).take(8)
      val last: NonEmptyVector[Double] = shifts.last.map(_ => 0.0)

      NonEmptyVector.fromIterable(NonEmptyVector.fromChunk(head).get, middle.toList :+ last)
    }

    val m: NonEmptyVector[Matrix[Double]] = {
      val matrixList =
        if (n <= 2) Data.hybrid_func2_M_D2
        else if (n <= 10) Data.hybrid_func2_M_D10
        else if (n <= 30) Data.hybrid_func2_M_D30
        else Data.hybrid_func2_M_D50

      matrixList.map(_.mapRow(_.take(n)))
    }

    val funcs = NonEmptyVector(
      (a: NonEmptyVector[Double]) => ackley(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => ackley(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => spherical(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => spherical(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons))
    )
    val b = NonEmptyVector(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
    val λ = NonEmptyVector(
      2 * 5.0 / 32.0,
      5.0 / 32.0,
      2.0 * 1.0,
      1.0,
      2 * 5.0 / 100.0,
      5.0 / 100.0,
      2.0 * 10.0,
      10.0,
      2.0 * 5.0 / 60.0,
      5.0 / 60.0
    )
    val σ = NonEmptyVector(1.0, 2.0, 1.5, 1.5, 1.0, 1.0, 1.5, 1.5, 2.0, 2.0)
    val h = Helper.hybrid(b, o, m, funcs, λ, σ)

    h(x) + fbias
  }

  def toAtLeast2List(x: NonEmptyVector[Double]): AtLeast2List =
    AtLeast2List.make(x) match {
        case ZValidation.Failure(_, e) => sys.error(e.toString())
        case ZValidation.Success(_, a) => a
      }

  private def expandedShafferF6(x: NonEmptyList[Double]): Double =
    mapSum(pairs(x.toList :+ x.head)) {
      case (a, b) => schaffer6(toAtLeast2List(NonEmptyVector.fromCons(x.toCons)))
    }

  def f8f2(x: NonEmptyList[Double]): Double =
    mapSum(pairs(x.toList :+ x.head)) {
      case (a, b) => {
        griewank(NonEmptyList(rosenbrock(toAtLeast2List(NonEmptyVector(a, b)))))
      }
    }

  /*
   * F21: Rotated Hybrid Composition Function
   * x ∈ [−5,5]D
   */
  def f21(x: AtLeast2List): Double = {
    // P.params match {
    //   case (o, m, fbias) => {
    val n = AtLeast2List.unwrap(x).length
    val fbias = 360.0

    val o = Data.hybrid_func3_data.limit(n).shiftVectors
    val m: NonEmptyVector[Matrix[Double]] = {
      val matrixList =
        if (n <= 2) Data.hybrid_func3_M_D2
        else if (n <= 10) Data.hybrid_func3_M_D10
        else if (n <= 30) Data.hybrid_func3_M_D30
        else Data.hybrid_func3_M_D50

      matrixList.map(_.mapRow(_.take(n)))
    }

    val funcs = NonEmptyVector(
      (a: NonEmptyVector[Double]) => expandedShafferF6(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => expandedShafferF6(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => f8f2(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => f8f2(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons))
    )
    val b = NonEmptyVector(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
    val λ = NonEmptyVector(
      5.0 * 5.0 / 100.0,
      5.0 / 100.0,
      5.0 * 1.0,
      1.0,
      5.0 * 1.0,
      1.0,
      5.0 * 10.0,
      10.0,
      5.0 * 5.0 / 200.0,
      5.0 / 200.0
    )
    val σ = NonEmptyVector(1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0)
    val h = Helper.hybrid(b, o, m, funcs, λ, σ)

    h(AtLeast2List.unwrap(x)) + fbias
//      }
//    }
  }

  /*
   * F22: Rotated Hybrid Composition Function with High Condition Number Matrix
   * x ∈ [−5,5]D
   */
  def f22(x: AtLeast2List): Double = {
    // P.params match {
    //   case (o, m, fbias) => {
    val n = AtLeast2List.unwrap(x).length
    val fbias = 360.0

    val o = Data.hybrid_func3_data.limit(n).shiftVectors
    val m: NonEmptyVector[Matrix[Double]] = {
      val matrixList =
        if (n <= 2) Data.hybrid_func3_M_D2
        else if (n <= 10) Data.hybrid_func3_M_D10
        else if (n <= 30) Data.hybrid_func3_M_D30
        else Data.hybrid_func3_M_D50

      matrixList.map(_.mapRow(_.take(n)))
    }

    val funcs = NonEmptyVector(
      (a: NonEmptyVector[Double]) => expandedShafferF6(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => expandedShafferF6(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => rastrigin(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => f8f2(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => f8f2(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => weierstrass(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons)),
      (a: NonEmptyVector[Double]) => griewank(NonEmptyList.fromCons(a.toCons))
    )
    val b = NonEmptyVector(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
    val λ = NonEmptyVector(
      5.0 * 5.0 / 100.0,
      5.0 / 100.0,
      5.0 * 1.0,
      1.0,
      5.0 * 1.0,
      1.0,
      5.0 * 10.0,
      10.0,
      5.0 * 5.0 / 200.0,
      5.0 / 200.0
    )
    val σ = NonEmptyVector(1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0)
    val h = Helper.hybrid(b, o, m, funcs, λ, σ)

    h(AtLeast2List.unwrap(x)) + fbias
  }


  /*
   * F23: Non-Continuous Rotated Hybrid Composition Function
   * x ∈ [−5,5]D
   */
  def f23(x: AtLeast2List): Double = {
    //(P21.params, P23.params) match {
    //      case ((o, _, f21bias), fbias) =>
    val n = AtLeast2List.unwrap(x).length
    val f21bias = 360.0
    val fbias = 360.0

    val o = Data.hybrid_func3_data.limit(n).shiftVectors

    val xModified = AtLeast2List.unwrap(x).zipWith(o.head)(Tuple2.apply) map {
      case (xj, o1j) =>
        if (abs(xj - o1j) < 0.5) xj
        else round(2.0 * xj) / 2.0
    }

    f21(toAtLeast2List(xModified)) - f21bias + fbias
  }

  /*
   * F24: Rotated Hybrid Composition Function
   * x ∈ [−5,5]D
   */
  def f24(x: AtLeast2List): RVar[Double] = {
    // P.params match {
    //   case (o, m, fbias, noise) =>
    def preRound(x: NonEmptyVector[Double]) =
      x map { xj =>
        if (abs(xj) < 0.5) xj
        else round(2.0 * xj) / 2.0
      }

    val fbias = 260.0
    val noise = Dist.stdNormal
    val n = AtLeast2List.unwrap(x).length

    val o = Data.hybrid_func4_data.limit(n).shiftVectors
    val m: NonEmptyVector[Matrix[Double]] = {
      val matrixList =
        if (n <= 2) Data.hybrid_func4_M_D2
        else if (n <= 10) Data.hybrid_func4_M_D10
        else if (n <= 30) Data.hybrid_func4_M_D30
        else Data.hybrid_func4_M_D50

      matrixList.map(_.mapRow(_.take(n)))
    }


    val funcs: NonEmptyVector[NonEmptyVector[Double] => RVar[Double]] =
      NonEmptyVector(
        (a: NonEmptyVector[Double]) => RVar.pure(weierstrass(NonEmptyList.fromCons(a.toCons))),
        (a: NonEmptyVector[Double]) => RVar.pure(expandedShafferF6(NonEmptyList.fromCons(a.toCons))),
        (a: NonEmptyVector[Double]) => RVar.pure(f8f2(NonEmptyList.fromCons(a.toCons))),
        (a: NonEmptyVector[Double]) => RVar.pure(ackley(NonEmptyList.fromCons(a.toCons))),
        (a: NonEmptyVector[Double]) => RVar.pure(rastrigin(NonEmptyList.fromCons(a.toCons))),
        (a: NonEmptyVector[Double]) => RVar.pure(griewank(NonEmptyList.fromCons(a.toCons))),
        (a: NonEmptyVector[Double]) => RVar.pure(expandedShafferF6(NonEmptyList.fromCons(preRound(a).toCons))),
        (a: NonEmptyVector[Double]) => RVar.pure(rastrigin(NonEmptyList.fromCons(preRound(a).toCons))),
        (a: NonEmptyVector[Double]) => RVar.pure(elliptic(toAtLeast2List(a))),
        (a: NonEmptyVector[Double]) =>
        noise.map { n =>
          spherical(NonEmptyList.fromCons(a.toCons)) * (1.0 + 0.1 * n)
        }
      )

    //val funcs: Vector[NonEmptyVector[Double] => RVar[Double]] = funcsSeq ///NonEmptyVector.fromIterableOption(funcsSeq).get
    val b = NonEmptyVector(0.0, 100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0, 900.0)
    val λ = NonEmptyVector(10.0, 5.0 / 20.0, 1.0, 5.0 / 32.0, 1.0, 5.0 / 100.0, 5.0 / 50.0, 1.0, 5.0 / 100.0, 5.0 / 100.0)
    val σ = NonEmptyVector(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0)
    val h = Helper.hybridR(b, o, m, funcs, λ, σ)

    h(AtLeast2List.unwrap(x)) map { _ + fbias }
  }

  /*
   * F25: Rotated Hybrid Composition Function without bounds
   * x ∈ [2, 5]D
   */
  def f25(x: AtLeast2List): RVar[Double] =
    f24(x)

  // //(P24.params, P25.params) match {
  //   //case ((_, _, f24bias, _), fbias) =>
  //   val f24bias = 260.0
  //   val fbias = 260.0
  //
  //   f24(x) map { _ - f24bias + fbias }
  // }

}
