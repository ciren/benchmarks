package benchmarks
package bbob2009

import _root_.scala.Predef.{any2stringadd => _, _}

import shapeless._
import shapeless.ops.nat._
import spire.algebra._
import spire.math._
import spire.implicits._

import cilib._
import scalaz.Scalaz._

import benchmarks.Benchmarks._
import benchmarks.implicits._
import dimension._
import matrix._
import Helpers._

/*
 * Based on: Real-Parameter Black-Box Optimization Benchmarking 2009:
 * Noiseless Functions Definitions
 *
 * by N. Hansen, S. Finck, R. Ros and A. Auger
*/
object Benchmarks {

  def penalty[N<:Nat,A:Order:Signed](x: Dimension[N,A])(implicit A: Field[A]): A =
    x mapSum { xi => max(A.fromDouble(0.0), abs(xi) - 5) ** 2 }

  /*
   * F1: Sphere Function
   * x ∈ [-5, 5]D
   */
  def f1[N<:Nat,A:Ring](x: Dimension[N,A])(implicit P: F1Params[N,A]): A =
    P.params match {
      case (o, fbias) => spherical(x - o) + fbias
    }

  /*
   * F2: Ellipsoidal Function
   * x ∈ [-5, 5]D
   */
  def f2[N<:Nat:GTEq2,A:Field:Order:Signed:Trig](x: Dimension[N,A])(implicit P: F2Params[N,A]): A =
    P.params match {
      case (o, fbias) => {
        val z = (x - o).map(oscillate[A])
        elliptic(z) + fbias
      }
    }

  /*
   * F3: Rastrigin Function
   * x ∈ [-5, 5]D
   */
  def f3[N<:Nat:GTEq1:ToInt,A:Field:NRoot:Order:Signed:Trig](x: Dimension[N,A])(implicit P: F3Params[N,A]): A =
    P.params match {
      case (o, fbias) =>
        val asymm = asymmetric[N,A](0.2)
        val m = Matrix.alpha[N,A](10.0)
        val z = asymm((x - o).map(oscillate[A])) rotate m
        rastrigin(z) + fbias
    }

  /*
   * F4: Buche-Rastrigin Function
   * x ∈ [-5, 5]D
   */
  def f4[N<:Nat,A:Order:Signed:Trig](x: Dimension[N,A])(implicit P: F4Params[N,A], A: Field[A]): A = {
    val D = x.size
    def scale(xi: A, i: Int): A = {
      val factor = A.fromDouble(10.0 ** (0.5 * (i / (D - 1))))
      if ((i % 2 == 0) && (xi > 0)) 10.0 * factor
      else factor
    }

    P.params match {
      case (o, fbias) =>
        val z = (x - o).zipWithIndex map { case (xi, i) =>
          oscillate[A](xi) * scale(xi, i)
        }
        rastrigin(z) + 100.0 * penalty(x) + fbias
    }
  }

  /*
   * F5: Linear slope Function
   * x ∈ [-5, 5]D
   */
  def f5[N<:Nat,A:Order:Signed](x: Dimension[N,A])(implicit P: F5Params[N,A], A: Field[A]): A = {
    val D = x.size
    P.params match {
      case (o, fbias) =>
        val s = o.zipWithIndex map { case (oi, i) =>
          sign(oi) * (10.0 ** (i / (D - 1.0)))
        }
        val z = (x zip o) map { case (xi, oi) =>
          val xoi = if (oi < 0) A.fromDouble(-5.0) else A.fromDouble(5.0)
          if ((xoi * xi) < 25) xi
          else xoi
        }
        val result = (z zip s) mapSum { case (zi, si) =>
          5.0 * abs(si) - (si * zi)
        }
        result + fbias
    }
  }

  /*
   * F6: Attractive Sector Function
   * x ∈ [-5, 5]D
   */
  def f6[N<:Nat:GTEq1:ToInt,A:Field:Order:NRoot:Signed:Trig](x: Dimension[N,A])(implicit P: F6Params[N,A]): A =
    P.params match {
      case (o, fbias, q, r) =>
        val alpha = Matrix.alpha[N,A](10.0)
        val z = (x - o) rotate (q |*| alpha |*| r)
        val s = z map { zi => if (zi > 0) 10 ** 2 else 1 }
        val result = (z zip s) mapSum { case (zi, si) => (si * zi) ** 2 }
        oscillate(result) ** 0.9 + fbias
    }

  /*
   * F7: Step Ellipsoidal Function
   * x ∈ [-5, 5]D
   */
  def f7[N<:Nat:GTEq1:ToInt:HasHead,A:Field:IsReal:Order:NRoot:Signed](x: Dimension[N,A])(implicit P: F7Params[N,A]): A =
    P.params match {
      case (o, fbias, q, r) =>
        val zhat = (x - o) rotate (Matrix.alpha[N,A](10) |*| r)

        val ztilde = zhat map { zi =>
          // in the paper it's:
          // zi > 0.5
          // not |zi| > 0.5 as in the C code
          if ((abs(zi) - 0.5) > 0) floor(0.5 + zi)
          else floor(0.5 + 10.0 * zi) / 10.0
        }
        val z = ztilde rotate q
        val D = z.size
        val left = abs(zhat.head) / (10.0 ** 4)
        val right = z.zipWithIndex mapSum { case (zi, i) =>
          (10.0 ** (2.0 * (i / (D - 1)))) * (zi ** 2)
        }
        0.1 * max(left, right) + penalty(x) + fbias
      }

  /*
   * F8: Rosenbrock Function
   * x ∈ [-5, 5]D
   */
  def f8[N<:Nat:GTEq2,A:Field](x: Dimension[N,A])(implicit P: F8Params[N,A]): A =
    P.params match {
      case (o, fbias) =>
        val D = x.size
        val factor = max(sqrt(D.toDouble) / 8.0, 1.0)
        val z = (x - o) map (_ * factor + 1.0)
        rosenbrock(z) + fbias
    }

  /*
   * F9: Rosenbrock Function rotated
   * x ∈ [-5, 5]D
   */
  def f9[N<:Nat:GTEq2,A:Field](x: Dimension[N,A])(implicit P: F9Params[N,A]): A =
    P.params match {
      case (fbias, r) =>
        val D = x.size
        val factor = max(sqrt(D.toDouble) / 8.0, 1.0)
        val z = (x rotate r) map (_ * factor + 0.5)
        rosenbrock(z) + fbias
    }

  /*
   * F10: Ellipsoidal Function
   * x ∈ [-5, 5]D
   */
  def f10[N<:Nat:GTEq2,A:Field:Order:Signed:Trig](x: Dimension[N,A])(implicit P: F10Params[N,A]): A =
    P.params match {
      case (o, fbias, r) =>
        val z = (x - o).rotate(r).map(oscillate[A])
        elliptic(z) + fbias
    }

  /*
   * F11: Discus Function
   * x ∈ [-5, 5]D
   */
  def f11[N<:Nat:HasHead,A:Field:Order:Signed:Trig](x: Dimension[N,A])(implicit P: F11Params[N,A]): A =
    P.params match {
      case (o, fbias, r) =>
        val z = ((x - o) rotate r).map(oscillate[A])
        discus(z) + fbias
    }

  /*
   * F12: Bent Cigar Function
   * x ∈ [-5, 5]D
   */
  def f12[N<:Nat:HasHead:Pred,A:Field:Order:NRoot](x: Dimension[N,A])(implicit P: F12Params[N,A]): A =
    P.params match {
      case (o, fbias, r) =>
        val asymm = asymmetric[N,A](0.5)
        val z = asymm((x - o) rotate r) rotate r
        (z.head ** 2) + (10.0 ** 6) * z.tail.mapSum(_ ** 2) + fbias
    }

  /*
   * F13: Sharp Ridge Function
   * x ∈ [-5, 5]D
   */
  def f13[N<:Nat:GTEq1:HasHead:Pred:ToInt,A:Field:NRoot](x: Dimension[N,A])(implicit P: F13Params[N,A]): A =
    P.params match {
      case (o, fbias, q, r) =>
        val rotation = q |*| Matrix.alpha[N,A](10.0) |*| r
        val z = (x - o) rotate rotation
        val term = z.tail mapSum (_ ** 2)
        (z.head ** 2) + 100.0 * sqrt(term) + fbias
    }

  /*
   * F14: Different Powers Function
   * x ∈ [-5, 5]D
   */
  def f14[N<:Nat,A:Field:NRoot:Signed](x: Dimension[N,A])(implicit P: F14Params[N,A]): A =
    P.params match {
      case (o, fbias, r) =>
        val D = x.size
        val z = (x - o) rotate r
        val result = z.zipWithIndex mapSum { case (zi, i) =>
          val power = 2.0 + 4.0 * (i / (D - 1.0))
          abs(zi) ** power
        }
        sqrt(result) + fbias
    }

  /*
   * F15: Rastrigin Function
   * x ∈ [-5, 5]D
   */
  def f15[N<:Nat:GTEq1:ToInt,A:Field:Order:NRoot:Signed:Trig](x: Dimension[N,A])(implicit P: F15Params[N,A]): A =
    P.params match {
      case (o, fbias, r, q) =>
        val rotation = r |*| Matrix.alpha[N,A](10) |*| q
        val asymm = asymmetric[N,A](0.2)
        val z = asymm(((x - o) rotate r).map(oscillate[A])) rotate rotation
        rastrigin(z) + fbias
    }

  /*
   * F16: Weierstrass Function
   * x ∈ [-5, 5]D
   */
  def f16[N<:Nat:GTEq1:ToInt,A:Field:NRoot:Order:Signed:Trig](x: Dimension[N,A])(implicit P: F16Params[N,A]): A = {
    def inner[B:Field:Trig](zi: B): B =
        (0 to 11) mapSum { ki =>
          (0.5 ** ki.toDouble) * cos(2.0 * pi * (3.0 ** ki.toDouble) * (zi + 0.5))
        }

    P.params match {
      case (o, fbias, r, q) =>
        val rotation = r |*| Matrix.alpha[N,A](1.0 / 100.0) |*| q
        val z = ((x - o) rotate r).map(oscillate[A]).rotate(rotation)
        val f0 = inner(0.0)
        val D = x.size.toDouble

        val result = ((z.mapSum(inner[A]) / D) - f0) ** 3
        10.0 * result + (10.0 * penalty(x) / D) + fbias
    }
  }

  private[this] def schaffersF7[N<:Nat:GTEq1:GTEq2:ToInt,A:Field:NRoot:Order:Signed:Trig]
    (condition: Double): (Dimension[N,A], A, Matrix[N,N,A], Matrix[N,N,A]) => Dimension[N,A] => A =
      (o, fbias, q, r) => x => {
        val rotation = Matrix.alpha[N,A](condition) |*| q
        val asymm = asymmetric[N,A](0.5)
        val z = asymm((x - o) rotate r) rotate rotation
        val s = z.pairs map { case (zi, zi1) => sqrt((zi ** 2) + (zi1 ** 2)) }

        val result = s mapSum { si => sqrt(si) * (1.0 + (sin(50.0 * (si ** 0.2)) ** 2)) }
        ((result / (x.size - 1.0)) ** 2) + 10.0 * penalty(x) + fbias
      }

  /*
   * F17: Schaffers F7 Function
   * x ∈ [-5, 5]D
   */
  def f17[N<:Nat:GTEq1:GTEq2:ToInt,A:Field:NRoot:Order:Signed:Trig](x: Dimension[N,A])(implicit P: F17Params[N,A]): A =
    P.params match {
      case (o, fbias, q, r) =>
        val f = schaffersF7[N,A](10.0)
        f(o, fbias, q, r)(x)
    }

  /*
   * F18: Schaffers F7 Function, moderately ill-conditioned
   * x ∈ [-5, 5]D
   */
  def f18[N<:Nat:GTEq1:GTEq2:ToInt,A:Field:NRoot:Order:Signed:Trig](x: Dimension[N,A])(implicit P: F18Params[N,A]): A =
    P.params match {
      case (o, fbias, q, r) =>
        val f = schaffersF7[N,A](1000.0)
        f(o, fbias, q, r)(x)
    }

  /*
   * F19: Composite Griewank-Rosenbrock Function F8F2
   * x ∈ [-5, 5]D
   */
  def f19[N<:Nat:GTEq2,A:Field:Trig](x: Dimension[N,A])(implicit P: F19Params[N,A]): A =
    P.params match {
      case (fbias, r) =>
        val D = x.size
        val factor = max(sqrt(D.toDouble) / 8.0, 1.0)
        val z = (x rotate r) map (_ * factor + 0.5)
        val s = z.pairs map { case (zi, zi1) =>
          100.0 * (((zi ** 2) - zi1) ** 2) + ((zi - 1.0) ** 2)
        }

        val result = s mapSum { si => (si / 4000.0) - cos(si) }
        (10.0 / (D - 1.0)) * result + 10.0 + fbias
    }

  /*
   * F20: Schwefel Function
   * x ∈ [-5, 5]D
   */
  def f20[N<:Nat:GTEq1:GTEq2:HasHead:ToInt:Pred,A:IsReal:Field:NRoot:Order:Signed:Trig](x: Dimension[N,A])(implicit P: F20Params[N,A]): A =
    P.params match {
      case (fbias, ones) =>
        val factor = 4.2096874633 / 2.0
        val xopt = ones map { _ * factor }
        val xhat = (x zip ones) map { case (xi, one) => 2.0 * one * xi }

        val xoptA = xopt.map(xi => 2 * abs(xi))
        val pairs = (xhat zip xoptA).pairs map { case ((xi, oi), (xi1, _)) =>
          xi1 + 0.25 * (xi - oi)
        }
        val zhat: Dimension[N,A] = Sized.wrap[IndexedSeq[A],N](xhat.head +: pairs.unsized)
        val alpha = Matrix.alpha[N,A](10.0)
        val z = (zhat - xoptA).rotate(alpha).zip(xoptA) map { case (zi, oi) =>
          (zi + oi) * 100.0
        }

        val result = z mapSum { zi => zi * sin(sqrt(abs(zi))) }
        val pen = z mapSum { zi =>
          val tmp = abs(zi) - 500.0
          if (tmp > 0)
            tmp ** 2
          else
            tmp * 0.0
        }

        0.01 * (pen + 418.9828872724339 - result / x.size) + fbias
    }

  private[this] def gallaghersPeaks[N<:Nat:GTEq1:ToInt,A:Field:NRoot:Order:Ordering:Signed:Trig]
    (peaks: Int): (A, Matrix[N,N,A]) => RVar[Dimension[N,A] => A] =
      (fbias, r) => {
        val w = (1 to peaks) map { i =>
          if (i == 1) 10.0
          else 1.1 + 8 * ((i - 2.0) / (peaks - 2))
        }

        val y = for {
          y1 <- Dimension random[N,A] Interval(-4.0, 4.0)
          ys <- Dimension random[N,A] Interval(-4.9, 4.9) replicateM (peaks - 1)
        } yield y1 +: ys

        val options = (0 to (peaks - 2)) map { j =>
          1000.0 ** (2 * (j / (peaks - 2.0)))
        }
        val C = for {
          shuffled <- RVar shuffle options.toList
          a         = 1000.0 +: shuffled
        } yield a map { ai =>
          val exponent = implicitly[Field[A]] fromDouble (ai ** 0.25)
          Matrix.alpha[N,A](ai) / exponent
        }

        val terms = for {
          ws <- RVar.point(w)
          ys <- y
          cs <- C
        } yield (x: Dimension[N,A]) => (ws zip ys zip cs) map { case ((wi, yi), ci) =>
          val rotation = (x - yi).t |*| r.t |*| ci |*| r
          val rotated  = rotation * (x - yi)
          val fac = -0.5 / implicitly[ToInt[N]].apply
          wi * exp(fac * rotated.head)
        }

        terms map { t => (x: Dimension[N,A]) =>
          oscillate(10.0 - t(x).max) ** 2 + penalty(x) + fbias
        }
      }

  /*
   * F21: Gallagher's Gaussian 101-me Peaks Function
   * x ∈ [-5, 5]D
   */
  def f21[N<:Nat:GTEq1:ToInt,A:Field:NRoot:Order:Ordering:Signed:Trig](implicit P: F21Params[N,A]): RVar[Dimension[N,A] => A] =
    P.params match {
      case (fbias, r) =>
        val f = gallaghersPeaks[N,A](101)
        f(fbias, r)
    }

  /*
   * F22: Gallagher's Gaussian 21-hi Peaks Function
   * x ∈ [-5, 5]D
   */
  def f22[N<:Nat:GTEq1:ToInt,A:Field:NRoot:Order:Ordering:Signed:Trig](implicit P: F22Params[N,A]): RVar[Dimension[N,A] => A] =
    P.params match {
      case (fbias, r) =>
        val f = gallaghersPeaks[N,A](21)
        f(fbias, r)
    }

  /*
   * F23: Katsuura Function
   * x ∈ [-5, 5]D
   */
  def f23[N<:Nat:GTEq1:ToInt,A:Field:IsReal:NRoot](x: Dimension[N,A])(implicit P: F23Params[N,A]): A =
    P.params match {
      case (o, fbias, q, r) =>
        val D = x.size
        val alpha = Matrix.alpha[N,A](100.0)
        val z = (x - o).rotate(q |*| alpha |*| r)

        val term = z.zipWithIndex mapProduct { case (zi, i) =>
          val t = (1 to 32) mapSum { j =>
            val innerFactor = 2 ** j
            abs(innerFactor * zi - floor(innerFactor * zi + 0.5)) / innerFactor
          }
          1 + (i + 1) * t
        }
        val term2 = term ** (10.0 / (D.toDouble ** 1.2))

        (10.0 / (D ** 2)) * (term2 - 1) + penalty(x) + fbias
    }

  /*
   * F24: Lunacek bi-Rastrigin Function
   * x ∈ [-5, 5]D
   */
  def f24[N<:Nat:GTEq1:ToInt,A:Field:NRoot:Order:Signed:Trig](x: Dimension[N,A])(implicit P: F24Params[N,A]): A =
    P.params match {
      case (fbias, ones, q, r) =>
        val D = x.size
        val d = 1
        val s = 1 - 1 / (2 * sqrt(D + 20.0) - 8.2)
        val μ0 = implicitly[Field[A]].fromDouble(2.5)
        val μ1 = -sqrt(((μ0 ** 2) - d) / s)
        val xopt = ones map { _ * μ0 }
        val xhat = (xopt zip x) map { case (oi, xi) => 2 * sign(oi) * xi }

        val inner = xhat map { _ - μ0 }
        val alpha = Matrix.alpha[N,A](100.0)
        val z = inner rotate (q |*| alpha |*| r)

        val left = xhat mapSum { xi => (xi - μ0) ** 2 }
        val right = d * D + s * xhat.mapSum { xi => (xi - μ1) ** 2 }

        val term1 = min(left, right)
        val term2 = D - z.mapSum { zi => cos(2 * pi * zi) }

        term1 + 10 * term2 + (10 ** 4) * penalty(x) + fbias
    }

}
