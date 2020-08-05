package benchmarks

import _root_.scala.Predef.{ any2stringadd => _, _ }
import dimension._
import implicits._
import scalaz.Scalaz._
import scalaz.{ Ordering => _, _ }
import shapeless._
import shapeless.ops.nat._
import spire.algebra.{ Field, IsReal, NRoot, Order, Ring, Signed, Trig }
import spire.implicits._
import spire.math._

object Benchmarks {

  def absoluteValue[N <: Nat, A: Ring: Signed](x: Dimension[N, A]) =
    x.mapSum(abs(_))

  def ackley[N <: Nat, A: Field: NRoot: Trig](x: Dimension[N, A]) = {
    val n      = x.size
    val sumcos = x.mapSum(xi => cos(2 * pi * xi))
    val sumsqr = x.mapSum(_ ** 2)

    -20 * exp(-0.2 * sqrt(sumsqr / n)) - exp(sumcos / n) + 20 + e
  }

  def ackley2[A: Field: NRoot: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    -200 * exp(-0.02 * sqrt((x1 ** 2) + (x2 ** 2)))
  }

  def ackley3[A: Field: NRoot: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    ackley2(x) + 5 * exp(cos(3 * x1) + sin(3 * x2))
  }

  def adjiman[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    cos(x1) * sin(x2) - (x1) / (x2 ** 2 + 1)
  }

  def alpine1[N <: Nat, A: Field: Signed: Trig](x: Dimension[N, A]) =
    x.mapSum(xi => abs((xi * sin(xi)) + (0.1 * xi)))

  def alpine2[N <: Nat, A: Field: NRoot: Trig](x: Dimension[N, A]) =
    x.mapProduct(xi => sqrt(xi) * sin(xi))

  def arithmeticMean[N <: Nat, A: Field: NRoot](x: Dimension[N, A]) = {
    val n        = x.size
    val avg      = x.mapSum(xi => xi) / n
    val rootProd = x.mapProduct(xi => xi) ** (1.0 / n)

    (avg - rootProd) ** 2
  }

  def bartelsConn[A: Ring: Signed: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    abs(x1 ** 2 + x2 ** 2 + x1 * x2) + abs(sin(x1)) + abs(cos(x2))
  }

  def beale[A: Field: IsReal](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (1.5 - x1 + x1 * x2) ** 2 +
      (2.25 - x1 + x1 * (x2 ** 2)) ** 2 +
      (2.625 - x1 + x1 * (x2 ** 3)) ** 2
  }

  def biggsEXP2[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (1 to 10) mapSum { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5 * exp(-10 * ti)
      (exp(-ti * x1) - 5 * exp(-ti * x2) - yi) ** 2
    }
  }

  def biggsEXP3[A: Field: Trig](x: Dimension3[A]) = {
    val (x1, x2, x3) = x.tuple
    (1 to 10) mapSum { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5 * exp(-10 * ti)
      (exp(-ti * x1) - x3 * exp(-ti * x2) - yi) ** 2
    }
  }

  def biggsEXP4[A: Field: Trig](x: Dimension4[A]) = {
    val (x1, x2, x3, x4) = x.tuple
    (1 to 10).mapSum { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5 * exp(-10 * ti)
      (x3 * exp(-ti * x1) - x4 * exp(-ti * x2) - yi) ** 2
    }
  }

  def biggsEXP5[A: Field: Trig](x: Dimension5[A]) = {
    val (x1, x2, x3, x4, x5) = x.tuple
    (1 to 11).mapSum { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5 * exp(-10 * ti) + 3 * exp(-4 * ti)
      (x3 * exp(-ti * x1) - x4 * exp(-ti * x2) + 3 * exp(-ti * x5) - yi) ** 2
    }
  }

  def biggsEXP6[A: Field: Trig](x: Dimension6[A]) = {
    val (x1, x2, x3, x4, x5, x6) = x.tuple
    (1 to 13).mapSum { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5 * exp(-10 * ti) + 3 * exp(-4 * ti)
      (x3 * exp(-ti * x1) - x4 * exp(-ti * x2) + x6 * exp(-ti * x5) - yi) ** 2
    }
  }

  def bird[A: Ring: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    sin(x1) * exp((1 - cos(x2)) ** 2) +
      cos(x2) * exp((1 - sin(x1)) ** 2) + (x1 - x2) ** 2
  }

  def bohachevsky1[A: Field: IsReal: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
      cos(3 * pi * x1) - 0.4 * cos(4 * pi * x2) + 0.7
  }

  def bohachevsky2[A: Field: IsReal: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
      cos(3 * pi * x1) * cos(4 * pi * x2) + 0.3
  }

  def bohachevsky3[A: Field: IsReal: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (x1 ** 2) + 2 * (x2 ** 2) - 0.3 *
      cos(3 * pi * x1 + 4 * pi * x2) + 0.3
  }

  def booth[A: IsReal: Ring](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (x1 + 2 * x2 - 7) ** 2 + (2 * x1 + x2 - 5) ** 2
  }

  def boxBettsQuadraticSum[A: Field: Trig](k: Int)(x: Dimension3[A]) = {
    val (x1, x2, x3) = x.tuple
    (1 to k).mapSum { i =>
      val co = -0.1 * i
      val t1 = exp(co * x1)
      val t2 = exp(co * x2)
      val t3 = (exp(co) - exp(-i.toDouble)) * x3
      (t1 - t2 - t3) ** 2
    }
  }

  def brad[A: Field](x: Dimension3[A]) = {
    val (x1, x2, x3) = x.tuple
    val y = List(
      0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39
    )

    y.zipWithIndex mapSum {
      case (yi, i) =>
        val ui = i + 1
        val vi = 16 - ui
        val wi = min(ui, vi)
        ((yi - x1 - ui) / (vi * x2 + wi * x3)) ** 2
    }
  }

  def braninRCOS1[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1 = (x2 - (5.1 / (4 * (pi ** 2))) * (x1 ** 2) +
      (5 / pi) * x1 - 6) ** 2
    val t2 = 10 * (1 - 1 / (8 * pi)) * cos(x1)
    t1 + t2 + 10
  }

  def braninRCOS2[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = (-1.275 * (x1 ** 2) / (pi ** 2) + (5 * x1) / pi + x2 - 6) ** 2
    val t2       = (10 - 5 / (4 * pi)) * cos(x1) * cos(x2)
    val t3       = log((x1 ** 2) + (x2 ** 2) + 1) + 10
    t1 + t2 + t3
  }

  def brent[N <: Nat, A: Ring: Trig](x: Dimension[N, A]) =
    x.mapSum(xi => (xi + 10) ** 2) + exp(-x.mapSum(_ ** 2))

  def brown[N <: Nat: GTEq2, A: Ring: NRoot](x: Dimension[N, A]) =
    x.pairs.mapSum {
      case (xi, xi1) => (xi ** 2).fpow((xi1 ** 2) + 1) + (xi1 ** 2).fpow((xi ** 2) + 1)
    }

  def bukin2[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    100 * (x2 - 0.01 * (x1 ** 2) + 1) ** 2 + 0.01 * ((x1 + 10) ** 2)
  }

  def bukin2Adapted[A: Field](x: Dimension2[A]) =
    bukin2(x) ** 2

  def bukin4[A: Field: Signed](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    100 * (x2 ** 2) + 0.01 * abs(x1 + 10)
  }

  def bukin6[A: Field: NRoot: Signed](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    100 * sqrt(abs(x2 - 0.01 * (x1 ** 2))) + 0.01 * abs(x1 + 10)
  }

  def carromTable[A: Field: NRoot: Signed: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val u        = cos(x1) * cos(x2)
    val v        = sqrt((x1 ** 2) + (x2 ** 2))
    -((u * exp(abs(1 - v / pi))) ** 2) / 30.0
  }

  def centralTwoPeakTrap[A: Field: Order](x1: A) =
    if (x1 < 0) implicitly[Field[A]].zero
    else if (x1 <= 10) (-160.0 / 10) * x1
    else if (x1 <= 15) (-160.0 / 5) * (15 - x1)
    else if (x1 <= 20) (-200.0 / 5) * (x1 - 15)
    else implicitly[Field[A]].zero - 200

  def chichinadze[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = (x1 ** 2) - (12 * x1) + 11
    val t2       = 10 * cos(pi * (x1 / 2)) + 8 * sin(5 * pi * x1)
    val t3       = ((1.0 / 5) ** 0.5) * exp(-0.5 * ((x2 - 0.5) ** 2))
    t1 + t2 - t3
  }

  def chungReynolds[N <: Nat, A: Ring](x: Dimension[N, A]) = x.mapSum(_ ** 2) ** 2

  def cigar[N <: Nat: GTEq2: HasHead: Pred, A: Field](condition: Double = 10e6)(x: Dimension[N, A]) =
    x.head ** 2 + x.toList.tail.head ** 2 * condition + x.toList.tail.tail.mapSum(_ ** 2) * condition

  def colville[A: Field](x: Dimension4[A]) = {
    val (x1, x2, x3, x4) = x.tuple
    val t1               = 100 * ((x1 - (x2 ** 2)) ** 2) + ((1 - x1) ** 2)
    val t2               = 90 * ((x4 - x3) ** 2) + ((1 - x3) ** 2)
    val t3               = 10.1 * (((x2 - 1) ** 2) + ((x4 - 1) ** 2))
    val t4               = 19.8 * (x2 - 1) * (x4 - 1)
    t1 + t2 + t3 + t4
  }

  def corana[A: Field: IsReal: Order: Signed](a: A = 0.05)(x: Dimension4[A]) = {
    val d = Sized(1, 1000, 10, 100)

    (x zip d).mapSum {
      case (xi, di) =>
        val zi = 0.2 * floor(abs(xi / 0.2) + 0.49999) * signum(xi)
        val vi = abs(xi - zi)

        if (abs(vi) < a)
          0.15 * ((zi - a * (signum(zi))) ** 2) * di
        else
          di * (xi ** 2)
    }
  }

  def cosineMixture[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    -0.1 * x.mapSum(xi => cos(5 * pi * xi)) + x.mapSum(_ ** 2)

  def crossInTray[N <: Nat, A: Field: NRoot: Signed: Trig](x: Dimension[N, A]) = {
    val t1 = x.mapProduct(xi => sin(xi))
    val t2 = exp(abs(100 - (sqrt(x.mapSum(_ ** 2)) / pi)))
    -0.0001 * ((abs(t1 * t2) + 1) ** 0.1)
  }

  def crossLegTable[N <: Nat, A: Field: NRoot: Signed: Trig](x: Dimension[N, A]) =
    -1 / (crossInTray(x) / -0.0001)

  def crossCrowned[N <: Nat, A: Field: NRoot: Signed: Trig](x: Dimension[N, A]) =
    -crossInTray(x)

  def csendes[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    if (x.toList.exists(_ == 0.0)) None
    else Some(x.mapSum(xi => (xi ** 6) * (2 + sin(1 / xi))))

  def cube[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    100 * ((x2 - (x1 ** 3)) ** 2) + ((1 - x1) ** 2)
  }

  def damavandi[A: Field: IsReal: Signed: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    if ((x1 != 2.0) && (x2 != 2.0)) {
      val numer   = sin(pi * (x1 - 2)) * sin(pi * (x2 - 2))
      val denom   = (pi ** 2) * (x1 - 2) * (x2 - 2)
      val factor1 = 1 - (abs(numer / denom) ** 5)
      val factor2 = 2 + ((x1 - 7) ** 2) + 2 * ((x2 - 7) ** 2)
      Some(factor1 * factor2)
    } else None
  }

  def deb1[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    -(1.0 / x.size) * x.mapSum(xi => sin(5 * pi * xi) ** 6)

  def deb2[N <: Nat, A: Field: NRoot: Trig](x: Dimension[N, A]) =
    deb1(Sized(x.mapSum(xi => (xi ** 0.75) - 0.05)))

  def decanomial[A: Field: Signed](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val coX1     = List(1, -20, 180, -960, 3360, -8064, 13340, -15360, 11520, -5120, 2624)
    val coX2     = List(1, 12, 54, 108, 81)

    def one(l: List[Int], xi: A) =
      abs(l.zipWithIndex.mapSum {
        case (ci, i) => ci * (xi ** (l.size - 1 - i))
      })

    0.001 * ((one(coX2, x2) + one(coX1, x1)) ** 2)
  }

  def deckkersAarts[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = (x.tuple._1 ** 2, x.tuple._2 ** 2)
    val t1       = (10 ** 5) * x1 + x2
    val t2       = (x1 + x2) ** 2
    val t3       = (1.0 / (10 ** 5)) * ((x1 + x2) ** 4)

    t1 - t2 + t3
  }

  def deflectedCorrugatedSpring[N <: Nat, A: Field: NRoot: Trig](K: A = 5)(x: Dimension[N, A]) = {
    val α     = 5
    val inner = x.mapSum(xi => (xi - α) ** 2)
    val outer = x.mapSum(xi => ((xi - α) ** 2) - cos(K * sqrt(inner)))
    0.1 * outer
  }

  def deVilliersGlasser1[A: Field: NRoot: Trig](x: Dimension4[A]) = {
    val (x1, x2, x3, x4) = x.tuple
    (1 to 24).mapSum { i =>
      val ti = 0.1 * (i - 1)
      val yi = 60.137 * (1.371 ** ti) * sin(3.112 * ti + 1.761)
      val t1 = x1 * (x2 ** ti)
      val t2 = sin(x3 * ti + x4)
      val t3 = yi
      (t1 * t2 - t3) ** 2
    }
  }

  def deVilliersGlasser2[A: Field: NRoot: Trig](x: Dimension5[A]) = {
    val (x1, x2, x3, x4, x5) = x.tuple
    (1 to 16).mapSum { i =>
      val ti = 0.1 * (i - 1)
      val yi = 53.81 * (1.27 ** ti) * tanh(3.012 * ti + sin(2.13 * ti)) *
        cos(exp(0.507) * ti)
      val t1 = x1 * (x2 ** ti)
      val t2 = tanh(x3 * ti + sin(x4 * ti))
      val t3 = cos(ti * exp(x5))
      val t4 = yi

      (t1 * t2 * t3 - t4) ** 2
    }
  }

  def differentPowers[N <: Nat: GTEq2, A: NRoot: Ring: Signed](x: Dimension[N, A]) = {
    val n = x.size
    val inner = x.zipWithIndex.mapSum {
      case (xi, i) => abs(xi) ** (2 + ((4 * i) / (n - 1)))
    }
    sqrt(inner)
  }

  def discus[N <: Nat: HasHead, A: Ring](x: Dimension[N, A]) =
    (10 ** 6) * (x.head ** 2) + x.toList.tail.mapSum(_ ** 2)

  def dixonPrice[N <: Nat: GTEq2: HasHead, A: Ring](x: Dimension[N, A]) = {
    val t1 = ((x.head - 1) ** 2)
    val t2 = x.pairs.zipWithIndex.mapSum {
      case ((xi, xi1), i) => (i + 2) * (((2 * (xi1 ** 2)) - xi) ** 2)
    }
    t1 + t2
  }

  def dolan[A: Field: Signed: Trig](x: Dimension5[A]) = {
    val (x1, x2, x3, x4, x5) = x.tuple
    val t1                   = (x1 + 1.7 * x2) * sin(x1)
    val t2                   = -1.5 * x3 - 0.1 * x4 * cos(x4 + x5 - x1)
    val t3                   = 0.2 * (x5 ** 2) - x2 - 1
    abs(t1 + t2 + t3)
  }

  def dropWave[N <: Nat, A: Field: NRoot: Trig](x: Dimension[N, A]) = {
    val sumsqr = x.mapSum(_ ** 2)
    -(1 + cos(12 * sqrt(sumsqr))) / (2 + 0.5 * sumsqr)
  }

  def easom[A: Field: IsReal: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    -cos(x1) * cos(x2) * exp(-((x1 - pi) ** 2 + (x2 - pi) ** 2))
  }

  def eggCrate[N <: Nat, A: Ring: Trig](x: Dimension[N, A]) =
    x.mapSum(_ ** 2) + 24 * x.mapSum(sin(_) ** 2)

  def eggHolder[N <: Nat: GTEq2, A: Field: NRoot: Signed: Trig](x: Dimension[N, A]) =
    x.pairs mapSum {
      case (x1, x2) =>
        -(x2 + 47) * sin(sqrt(abs(x2 + (x1 / 2) + 47))) - x1 * sin(sqrt(abs(x1 - x2 - 47)))
    }

  def elAttarVidyasagarDutta[A: Ring](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = ((x1 ** 2) + x2 - 10) ** 2
    val t2       = (x1 + (x2 ** 2) - 7) ** 2
    val t3       = ((x1 ** 2) + (x2 ** 3) - 1) ** 2
    t1 + t2 + t3
  }

  def elliptic[N <: Nat: GTEq2, A: Field](x: Dimension[N, A]) = {
    val n = x.size
    x.zipWithIndex.mapSum {
      case (xi, i) => (1e6 ** (i / (n - 1.0))) * (xi ** 2)
    }
  }

  def equalMaxima[A: Field: Trig](x: Dimension1[A]) =
    sin(5 * pi * x.head) ** 6

  def exponential1[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    -exp(-0.5 * x.mapSum(_ ** 2))

  def exponential2[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (0 to 9).mapSum { i =>
      val t1 = 1 * exp(-i * x1 / 10)
      val t2 = 5 * exp(-i * x2 / 10)
      val t3 = 1 * exp(-i / 10.0)
      val t4 = 5 * exp(-i.toDouble)
      (t1 - t2 - t3 + t4) ** 2
    }
  }

  def fiveUnevenPeakTrap[A: Field: Order](x: Dimension1[A]) = x.head match {
    case xi if xi - 2.5 < 0  => 80 * (2.5 - xi)
    case xi if xi - 5.0 < 0  => 64 * (xi - 2.5)
    case xi if xi - 7.5 < 0  => 64 * (7.5 - xi)
    case xi if xi - 12.5 < 0 => 28 * (xi - 7.5)
    case xi if xi - 17.5 < 0 => 28 * (17.5 - xi)
    case xi if xi - 22.5 < 0 => 32 * (xi - 17.5)
    case xi if xi - 27.5 < 0 => 32 * (27.5 - xi)
    case xi                  => 80 * (xi - 27.5)
  }

  def freudensteinRoth[A: Ring](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = (x1 - 13 + ((5 - x2) * x2 - 2) * x2) ** 2
    val t2       = (x1 - 29 + ((x2 + 1) * x2 - 14) * x2) ** 2
    t1 + t2
  }

  def gear[A: Field: IsReal](x: Dimension4[A]) = {
    val (x1, x2, x3, x4) = x.map(floor(_)).tuple
    val t1               = 1 / 6.931
    val numer            = x1 * x2
    val denom            = x3 * x4
    (t1 - (numer / denom)) ** 2
  }

  def giunta[A: Field: Trig](x: Dimension2[A]) =
    0.6 + x.mapSum { xi =>
      val factor = (16.0 / 15.0) * xi - 1
      val t1     = sin(factor)
      val t2     = t1 ** 2
      val t3     = (1.0 / 50.0) * sin(4 * factor)
      t1 + t2 + t3
    }

  def goldsteinPrice1[A: Ring](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1 = 1 + ((x1 + x2 + 1) ** 2) * (19 - 14 * x1 + 3 * (x1 ** 2) -
      14 * x2 + 6 * x1 * x2 + 3 * (x2 ** 2))
    val t2 = 30 + ((2 * x1 - 3 * x2) ** 2) * (18 - 32 * x1 + 12 *
      (x1 ** 2) + 48 * x2 - 36 * x1 * x2 + 27 * (x2 ** 2))
    t1 * t2
  }

  def goldsteinPrice2[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = exp(0.5 * (((x1 ** 2) + (x2 ** 2) - 25) ** 2))
    val t2       = sin(4 * x1 - 3 * x2) ** 4
    val t3       = 0.5 * ((2 * x1 + x2 - 10) ** 2)
    t1 + t2 + t3
  }

  def griewank[N <: Nat, A: Field: NRoot: Trig](x: Dimension[N, A]) = {
    val prod = x.zipWithIndex.mapProduct {
      case (xi, i) =>
        cos(xi / sqrt(i + 1.0))
    }

    1 + x.mapSum(_ ** 2) / 4000.0 - prod
  }

  def gulf[A: Field: NRoot: Signed: Trig](x: Dimension3[A]) = {
    val (x1, x2, x3) = x.tuple

    (1 to 99).mapSum { i =>
      val ui    = 25 + (-50 * log(0.01 * i)) ** (2.0 / 3.0)
      val numer = abs(ui - x2).fpow(x3)
      (exp(-numer / x1) - (i / 100.0)) ** 2
    }
  }

  def hansen[A: Ring: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = (0 to 4).mapSum(i => (i + 1) * cos(i * x1 + i + 1))
    val t2       = (0 to 4).mapSum(j => (j + 1) * cos((j + 2) * x2 + j + 1))
    t1 * t2
  }

  def hartman3[A: Field: Trig](x: Dimension3[A]) = {
    val a = Sized(
      List(3.0, 10.0, 30.0),
      List(0.1, 10.0, 35.0),
      List(3.0, 10.0, 30.0),
      List(0.1, 10.0, 35.0)
    )

    val c = Sized(1.0, 1.2, 3.0, 3.2)

    val p = Sized(
      Sized(0.6890, 0.1170, 0.2673),
      Sized(0.4699, 0.4387, 0.7470),
      Sized(0.1091, 0.8732, 0.5547),
      Sized(0.0381, 0.5743, 0.8828)
    )

    -(a zip c zip p).mapSum {
      case ((ai, ci), pi) =>
        val power = (ai zip pi zip x) mapSum {
          case ((aij, pij), xj) =>
            aij * ((xj - pij) ** 2)
        }
        ci * exp(-power)
    }
  }

  def hartman6[A: Field: Trig](x: Dimension6[A]) = {
    val a = Sized(
      List(10.0, 3.00, 17.0, 3.50, 1.70, 8.00),
      List(0.05, 10.0, 17.0, 0.10, 8.00, 14.0),
      List(3.00, 3.50, 1.70, 10.0, 17.0, 8.00),
      List(17.0, 8.00, 0.05, 10.0, 0.10, 14.0)
    )

    val c = Sized(1.0, 1.2, 3.0, 3.2)

    val p = Sized(
      Sized(0.1312, 0.1696, 0.5569, 0.0124, 0.8283, 0.5886),
      Sized(0.2329, 0.4135, 0.8307, 0.3736, 0.1004, 0.9991),
      Sized(0.2348, 0.1451, 0.3522, 0.2883, 0.3047, 0.6650),
      Sized(0.4047, 0.8828, 0.8732, 0.5743, 0.1091, 0.0381)
    )

    -(a zip c zip p).mapSum {
      case ((ai, ci), pi) =>
        val power = (ai zip pi zip x).mapSum {
          case ((aij, pij), xj) =>
            aij * ((xj - pij) ** 2)
        }
        ci * exp(-power)
    }
  }

  def helicalValley[A: Field: NRoot: Order: Trig](x: Dimension3[A]) = {
    val (x1, x2, x3) = x.tuple
    val r            = sqrt((x1 ** 2) + (x2 ** 2))
    val θ            = 1 / (2 * pi) * atan2(x2, x1)
    (x3 ** 2) + 100 * ((x3 - 10 * θ) ** 2 + (r - 1) ** 2)
  }

  def himmelblau[A: Ring](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (x1 ** 2 + x2 - 11) ** 2 + (x1 + x2 ** 2 - 7) ** 2
  }

  def hosaki[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = 1 - 8 * x1 + 7 * (x1 ** 2)
    val t2       = (7.0 / 3.0) * (x1 ** 3)
    val t3       = (1.0 / 4.0) * (x1 ** 4)
    val t4       = (x2 ** 2) * exp(-x2)
    (t1 - t2 + t3) * t4
  }

  def hyperEllipsoid[N <: Nat, A: Ring](x: Dimension[N, A]) =
    x.zipWithIndex.mapSum { case (xi, i) => i * (xi ** 2) }

  def hyperEllipsoidRotated[N <: Nat, A: Ring](x: Dimension[N, A]) = {
    val y      = x.toList
    val values = (1 to x.size).toList.map(y take _)
    values.mapSum(_.mapSum(xi => xi ** 2))
  }

  def jennrichSampson[A: Ring: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (1 to 10).mapSum { i =>
      val t1 = 2 + 2 * i
      val t2 = exp(i * x1) + exp(i * x2)
      (t1 - t2) ** 2
    }
  }

  def judge[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val A = List(
      4.284, 4.149, 3.877, 0.533, 2.211, 2.389, 2.145, 3.231, 1.998, 1.379, 2.106, 1.428, 1.011, 2.179, 2.858, 1.388,
      1.651, 1.593, 1.046, 2.152
    )

    val B = List(
      0.286, 0.973, 0.384, 0.276, 0.973, 0.543, 0.957, 0.948, 0.543, 0.797, 0.936, 0.889, 0.006, 0.828, 0.399, 0.617,
      0.939, 0.784, 0.072, 0.889
    )

    val C = List(
      0.645, 0.585, 0.310, 0.058, 0.455, 0.779, 0.259, 0.202, 0.028, 0.099, 0.142, 0.296, 0.175, 0.180, 0.842, 0.039,
      0.103, 0.620, 0.158, 0.704
    )

    val mappedB = B.map(_ * x2)
    val mappedC = C.map(_ * (x2 ** 2))

    val t1 = (mappedB zip mappedC).map { case (ai, bi) => ai + bi }
    val t2 = t1.map(_ + x1)

    (t2 zip A).mapSum {
      case (t2, ai) => (t2 - ai) ** 2
    }
  }

  def katsuura[N <: Nat, A: Field: IsReal: NRoot](x: Dimension[N, A]) =
    x.zipWithIndex.mapProduct {
      case (xi, i) =>
        val t1 = i + 1
        val d  = 32
        val t2 = (1 to d).mapSum(k => floor((2 ** k) * xi) * (1.0 / (2 ** k)))
        1 + t1 * t2
    }

  def keane[A: Field: NRoot: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val numer    = (sin(x1 - x2) ** 2) * (sin(x1 + x2) ** 2)
    val denom    = sqrt((x1 ** 2) + (x2 ** 2))
    numer / denom
  }

  def kowalik[A: Field](x: Dimension4[A]) = {
    val (x1, x2, x3, x4) = x.tuple
    val b = Sized(
      4.0,
      2.0,
      1.0,
      0.5,
      0.25,
      1.0 / 6.0,
      0.125,
      0.1,
      1.0 / 12.0,
      1.0 / 14.0,
      0.0625
    )
    val a = Sized(
      0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 0.0323, 0.0235, 0.0246
    )
    (a zip b).mapSum {
      case (ai, bi) =>
        val numer = x1 * ((bi ** 2) + bi * x2)
        val denom = (bi ** 2) + bi * x3 + x4
        (ai - (numer / denom)) ** 2
    }
  }

  def langermann[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val a        = Sized(3, 5, 2, 1, 7)
    val b        = Sized(5, 2, 1, 4, 9)
    val c        = Sized(1, 2, 5, 2, 3)

    -(a zip b zip c).mapSum {
      case ((ai, bi), ci) =>
        val t1    = (x1 - ai) ** 2
        val t2    = (x2 - bi) ** 2
        val numer = ci * cos(pi * (t1 + t2))
        val denom = exp((t1 + t2) / pi)
        numer / denom
    }
  }

  def leon[A: Ring](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = 100 * ((x2 - (x1 ** 2)) ** 2)
    val t2       = (1 - x1) ** 2
    t1 + t2
  }

  def levy3[N <: Nat: GTEq2: HasHead, A: Field: Trig](x: Dimension[N, A]) = {
    def y(xi: A): A = 1 + (xi - 1) / 4.0
    val t1          = sin(pi * y(x.head)) ** 2
    val t2 = x.pairs.mapSum {
      case (xi, xi1) =>
        ((y(xi) - 1) ** 2) * (1 + 10 * ((pi * y(xi1) ** 2)))
    }
    val t3 = (y(x.last) - 1) ** 2
    t1 + t2 + t3
  }

  def levy5[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = (1 to 5).mapSum(i => i * cos((i - 1) * x1 + i))
    val t2       = (1 to 5).mapSum(j => j * cos((j + 1) * x2 + j))
    val t3       = (x1 + 1.42513) ** 2
    val t4       = (x2 + 0.80032) ** 2
    t1 * t2 + t3 + t4
  }

  def levy13[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = ((x1 - 1) ** 2) * ((sin(3 * pi * x2) ** 2) + 1)
    val t2       = ((x2 - 1) ** 2) * ((sin(2 * pi * x2) ** 2) + 1)
    val t3       = sin(3 * pi * x1) ** 2
    t1 + t2 + t3
  }

  def levyMontalvo2[N <: Nat: GTEq2: HasHead, A: Field: Trig](x: Dimension[N, A]) = {
    val t1 = sin(3 * pi * x.head) ** 2
    val t2 = x.pairs.mapSum {
      case (xi, xi1) =>
        ((xi - 1) ** 2) * ((sin(3 * pi * xi1) ** 2) + 1)
    }
    val t3 = ((x.last - 1) ** 2) * ((sin(2 * pi * x.last) ** 2) + 1)
    0.1 * (t1 + t2 + t3)
  }

  def matyas[A: Field: IsReal](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    0.26 * (x1 ** 2 + x2 ** 2) - 0.48 * x1 * x2
  }

  def maximum[N <: Nat, A: Ordering](x: Dimension[N, A]) =
    x.max

  def mcCormick[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = sin(x1 + x2) + ((x1 - x2) ** 2)
    val t2       = -1.5 * x1 + 2.5 * x2 + 1
    t1 + t2
  }

  def michalewicz[N <: Nat, A: Field: IsReal: NRoot: Trig](m: Double = 10.0)(x: Dimension[N, A]) =
    -x.zipWithIndex.mapSum {
      case (xi, i) =>
        sin(xi) * (sin(((i + 1) * (xi ** 2)) / pi) ** (2 * m))
    }

  def mieleCantrell[A: Field: Trig](x: Dimension4[A]) = {
    val (x1, x2, x3, x4) = x.tuple
    val t1               = (exp(-x1) - x2) ** 4
    val t2               = 100 * ((x2 - x3) ** 6)
    val t3               = tan(x3 - x4) ** 4
    val t4               = x1 ** 8
    t1 + t2 + t3 + t4
  }

  def minimum[N <: Nat, A: Ordering](x: Dimension[N, A]) =
    x.min

  def mishra1[N <: Nat, A: Ring: NRoot](x: Dimension[N, A]) = {
    val sum = x.toList.init.mapSum(xi => xi)
    val n   = x.size
    (1 + n - sum).fpow(n - sum)
  }

  def mishra2[N <: Nat: GTEq2, A: Field: NRoot](x: Dimension[N, A]) = {
    val sum = x.pairs.mapSum {
      case (xi, xi1) => 0.5 * (xi + xi1)
    }
    val n = x.size
    (1 + n - sum).fpow(n - sum)
  }

  def mishra3[A: Field: NRoot: Signed: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = 0.01 * (x1 + x2)
    val t2       = sqrt(abs(cos(sqrt(abs((x1 ** 2) + (x2 ** 2))))))
    t1 + t2
  }

  def mishra4[A: Field: NRoot: Signed: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = 0.01 * (x1 + x2)
    val t2       = sqrt(abs(sin(sqrt(abs((x1 ** 2) + (x2 ** 2))))))
    t1 + t2
  }

  def mishra5[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = sin((cos(x1) + cos(x2)) ** 2) ** 2
    val t2       = cos((sin(x1) + sin(x2)) ** 2) ** 2
    val t3       = 0.01 * (x1 + x2)
    ((t1 + t2 + x1) ** 2) + t3
  }

  def mishra6[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val a        = 0.1 * ((x1 - 1) ** 2 + (x2 - 1) ** 2)
    val u        = (cos(x1) + cos(x2)) ** 2
    val v        = (sin(x1) + sin(x2)) ** 2
    a - log((sin(u) ** 2 - cos(v) ** 2 + x1) ** 2)
  }

  def mishra7[N <: Nat, A: Field](x: Dimension[N, A]) = {
    def factorial(n: Int, accu: Int): Int = n match {
      case 0 => accu
      case _ => factorial(n - 1, n * accu)
    }
    val n    = x.size
    val prod = x.mapProduct(xi => xi)
    (prod - factorial(n, 1)) ** 2
  }

  def mishra8[A: Field: Signed](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val coX1     = Sized(1, -20, 180, -960, 3360, -8064, 13340, -15360, 11520, -5120, 2624)
    val coX2     = Sized(1, 12, 54, 108, 81)

    val t1 = abs(coX1.zipWithIndex.mapSum {
      case (ci, i) => ci * (x1 ** (coX1.length - 1 - i))
    })

    val t2 = abs(coX2.zipWithIndex.mapSum {
      case (ci, i) => ci * (x2 ** (coX2.length - 1 - i))
    })

    0.001 * ((t1 * t2) ** 2)
  }

  def mishra9[A: Field](x: Dimension3[A]) = {
    val (x1, x2, x3) = x.tuple
    val a            = (2 * x1 ** 3 + 5 * x1 * x2 + 4 * x3 - 2 * x1 ** 2 * x3 - 18)
    val b            = x1 + x2 ** 3 + x1 * x2 ** 2 + x1 * x3 ** 2 - 22
    val c            = (8 * x1 ** 2 + 2 * x2 * x3 + 2 * x2 ** 2 + 3 * x2 ** 3 - 52)
    ((a * (b ** 2) * c) + (a * b * (c ** 2)) + (b ** 2) + ((x1 + x2 - x3) ** 2)) ** 2
  }

  def mishra10[A: Field: IsReal](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val f1       = floor(x1) + floor(x2)
    val f2       = floor(x1) * floor(x2)
    (f1 - f2) ** 2
  }

  def mishra11[N <: Nat, A: Field: NRoot: Signed](x: Dimension[N, A]) = {
    val n  = x.size
    val t1 = (1.0 / n) * x.mapSum(abs(_))
    val t2 = x.mapProduct(abs(_)) ** (1.0 / n)
    (t1 - t2) ** 2
  }

  def multiModal[N <: Nat, A: Field: Signed](x: Dimension[N, A]) =
    x.mapProduct(abs(_)) * x.mapSum(abs(_))

  def needleEye[N <: Nat, A: Order: Signed](eye: A = 0.0001)(x: Dimension[N, A])(implicit A: Ring[A]) =
    if (x.toList.forall(xi => abs(xi) < eye)) A.one
    else if (x.toList.forall(xi => abs(xi) > eye)) x.mapSum(xi => 100 + abs(xi))
    else A.zero

  def newFunction1[A: Field: NRoot: Signed: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    abs(cos(sqrt(abs((x1 ** 2) + x2)))) ** 0.5 + 0.01 * (x1 + x2)
  }

  def newFunction2[A: Field: NRoot: Signed: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    abs(sin(sqrt(abs((x1 ** 2) + x2)))) ** 0.5 + 0.01 * (x1 + x2)
  }

  def norwegian[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    x.mapProduct(xi => cos(pi * (xi ** 3)) * ((99 + xi) / (100)))

  def parsopoulus[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (cos(x1) ** 2) + (sin(x2) ** 2)
  }

  def pathological[N <: Nat: GTEq2, A: Field: NRoot: Trig](x: Dimension[N, A]) =
    x.pairs.mapSum {
      case (xi, xi1) =>
        val numer = sin(sqrt(100 * (xi ** 2) + (xi1 ** 2))) ** 2 - 0.5
        val denom = 1 + 0.001 * (((xi ** 2) - 2 * xi * xi1 + (xi1 ** 2)) ** 2)
        0.5 + numer / denom
    }

  def paviani[A: Field: NRoot: Trig](x: Dimension10[A]) = {
    val t1 = x.mapSum(xi => (log(10 - xi) ** 2) + (log(xi - 2) ** 2))
    val t2 = x.mapProduct(xi => xi) ** 0.2
    t1 - t2
  }

  def penalty1[N <: Nat: GTEq2: HasHead, A: Order: Trig](x: Dimension[N, A])(implicit A: Field[A]) = {
    def u(xi: A, a: Int, k: Int, m: Int) =
      if (xi > a) k * ((xi - a) ** m)
      else if (xi < -a) k * ((-xi - a) ** m)
      else A.zero

    def yi(xi: A) = 1 + ((xi + 1) / 4)

    val term1 = 10 * (sin(pi * yi(x.head)) ** 2)
    val term2 = x.pairs.mapSum {
      case (xi, xi1) =>
        val t1 = (yi(xi) - 1) ** 2
        val t2 = 1 + 10 * (sin(pi * yi(xi1)) ** 2)
        t1 * t2
    }
    val term3 = (yi(x.last) - 1.0) ** 2
    val term4 = x.mapSum(xi => u(xi, 10, 100, 4))

    (pi / 30) * (term1 + term2 + term3) + term4
  }

  def penalty2[N <: Nat: GTEq2: HasHead, A: Order: Trig](x: Dimension[N, A])(implicit A: Field[A]) = {
    def u(xi: A, a: Int, k: Int, m: Int) =
      if (xi > a) k * ((xi - a) ** m)
      else if (xi < -a) k * ((-xi - a) ** m)
      else A.zero

    val term1 = sin(3.0 * pi * x.head) ** 2
    val term2 = x.pairs.mapSum {
      case (xi, xi1) =>
        val t1 = (xi - 1) ** 2
        val t2 = 1 + (sin(3 * pi * xi1) ** 2)
        t1 * t2
    }

    val term3 = ((x.last - 1) ** 2) * (1 + sin(2 * pi * x.last) ** 2)
    val term4 = x.mapSum(xi => u(xi, 5, 100, 4))
    0.1 * (term1 + term2 + term3) + term4
  }

  def penHolder[A: Field: NRoot: Signed: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = abs(1 - (sqrt((x1 ** 2) + (x2 ** 2)) / pi))
    val t2       = cos(x1) * cos(x2)
    val expon    = abs(exp(t1) * t2) ** -1
    -exp(-expon)
  }

  def periodic[N <: Nat, A: Field: Trig](x: Dimension[N, A]) = {
    val t1 = x.mapSum(sin(_) ** 2)
    val t2 = 0.1 * exp(-x.mapSum(_ ** 2))
    1 + t1 - t2
  }

  def pinter[N <: Nat: GTEq2: HasHead, A: Field: Trig](x: Dimension[N, A]) = {
    val padded                 = Sized(x.last) ++ (x :+ x.head)
    def A(a0: A, a1: A, a2: A) = a0 * sin(a1) + sin(a2)
    def B(b0: A, b1: A, b2: A) = (b0 ** 2) - (2 * b1) + (3 * b2) - cos(b1) + 1
    val t1                     = x.zipWithIndex.mapSum { case (xi, i) => (i + 1) * (xi ** 2) }
    val t2 = padded.sliding(3).toList.zipWithIndex.mapSum {
      case (Seq(x0, x1, x2), i) => 20 * (i + 1) * (sin(A(x0, x1, x2)) ** 2)
    }
    val t3 = padded.sliding(3).toList.zipWithIndex.mapSum {
      case (Seq(x0, x1, x2), i) => (i + 1) * log(1 + (i + 1) * (B(x0, x1, x2) ** 2))
    }
    t1 + t2 + t3
  }

  def plateau[N <: Nat, A: IsReal: Ring](x: Dimension[N, A]) =
    30 + x.mapSum(xi => floor(abs(xi)))

  def powell[A: Field](x: Dimension4[A]) = {
    val (x1, x2, x3, x4) = x.tuple
    val t1               = (x3 + 10 * x1) ** 2
    val t2               = 5 * ((x2 - x4) ** 2)
    val t3               = (x1 - 2 * x2) ** 4
    val t4               = 10 * ((x3 - x4) ** 4)
    t1 + t2 + t3 + t4
  }

  def powellSum[N <: Nat, A: Ring: Signed](x: Dimension[N, A]) =
    x.zipWithIndex.mapSum {
      case (xi, i) => abs(xi) ** (i + 1)
    }

  def powerSum[A: Ring](x: Dimension4[A]) = {
    val b = Sized(8, 18, 44, 114)
    b.zipWithIndex.mapSum {
      case (bk, i) =>
        val k = i + 1
        val t = x.mapSum(xi => xi ** k)
        (t - bk) ** 2
    }
  }

  def price1[N <: Nat, A: Ring: Signed](x: Dimension[N, A]) =
    x.mapSum(xi => (abs(xi) - 5) ** 2)

  def price2[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    1 + x.mapSum(xi => sin(xi) ** 2) - 0.1 * exp(-x.mapSum(_ ** 2))

  def price3[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = 100 * ((x2 - (x1 ** 2)) ** 2)
    val t2       = (6.4 * ((x2 - 0.5) ** 2) - x1 - 0.6) ** 2
    t1 + t2
  }

  def price4[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = (2 * (x1 ** 3) * x2 - (x2 ** 3)) ** 2
    val t2       = (6 * x1 - (x2 ** 2) + x2) ** 2
    t1 + t2
  }

  def qing[N <: Nat, A: Field](x: Dimension[N, A]) =
    x.zipWithIndex.mapSum {
      case (xi, i) => ((xi ** 2) - (i + 1)) ** 2
    }

  def quadratic[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = -3803.84 - 138.08 * x1
    val t2       = -232.92 * x2 + 128.08 * (x1 ** 2)
    val t3       = 203.64 * (x2 ** 2) + 182.25 * x1 * x2
    t1 + t2 + t3
  }

  def quadric[N <: Nat, A: Ring](x: Dimension[N, A]) =
    (1 to x.size).mapSum { i =>
      (x.toList take i).mapSum(xi => xi) ** 2
    }

  def quintic[N <: Nat, A: Field: Signed](x: Dimension[N, A]) =
    abs(x mapSum { xi =>
      (xi ** 5) - 3 * (xi ** 4) + 4 * (xi ** 3) + 2 * (xi ** 2) - 10 * xi - 4
    })

  def rana[N <: Nat: GTEq2, A: Field: NRoot: Signed: Trig](x: Dimension[N, A]) =
    x.pairs.mapSum {
      case (xi, xi1) =>
        val t1 = sqrt(abs(xi1 + xi + 1))
        val t2 = sqrt(abs(xi1 - xi + 1))
        (xi1 + 1) * cos(t2) * sin(t1) + xi * cos(t1) * sin(t2)
    }

  def rastrigin[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    10 * x.size + x.mapSum(xi => xi ** 2 - 10 * cos(2 * pi * xi))

  def rosenbrock[N <: Nat: GTEq2, A: Ring](x: Dimension[N, A]) =
    x.toList.pairs.mapSum {
      case (xi, xi1) => 100 * ((xi1 - (xi ** 2)) ** 2) + ((xi - 1) ** 2)
    }

  def ripple1[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    x.mapSum { xi =>
      val u = -2 * log(2) * (((xi - 0.1) / 0.8) ** 2)
      val v = (sin(5 * pi * xi) ** 6) + 0.1 * (cos(500 * pi * xi) ** 2)
      -exp(u) * v
    }

  def ripple2[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    x.mapSum { xi =>
      val u = -2 * log(2) * (((xi - 0.1) / 0.8) ** 2)
      val v = (sin(5 * pi * xi) ** 6)
      -exp(u) * v
    }

  def rotatedEllipse1[N <: Nat: GTEq2, A: Field](x: Dimension[N, A]) =
    x.pairs.mapSum {
      case (x1, x2) =>
        (7 * (x1 ** 2)) - (6 * sqrt(3.0) * x1 * x2) + (13 * (x2 ** 2))
    }

  def rotatedEllipse2[N <: Nat: GTEq2, A: Ring](x: Dimension[N, A]) =
    x.pairs.mapSum {
      case (x1, x2) => (x1 ** 2) - (x1 * x2) + (x2 ** 2)
    }

  def salomon[N <: Nat, A: Field: NRoot: Trig](x: Dimension[N, A]) = {
    val ss = sqrt(spherical(x))
    -cos(2 * pi * ss) + (0.1 * ss) + 1
  }

  def sargan[N <: Nat, A: Field](x: Dimension[N, A]) = {
    val zipped = x.zipWithIndex
    zipped.mapSum {
      case (xi, i) =>
        val sum = zipped.toList.filter { case (_, j) => i != j }.mapSum { case (xj, _) => xi * xj }
        x.size * ((xi ** 2) + 0.4 * sum)
    }
  }

  def schaffer1[N <: Nat: GTEq2, A: Field: Trig](x: Dimension[N, A]) =
    x.pairs.mapSum {
      case (xi, xi1) =>
        val t1 = (xi ** 2) + (xi1 ** 2)
        val t2 = (xi ** 2) + (xi1 ** 2)
        val t3 = (sin((t1) ** 2) ** 2) - 0.5
        val t4 = (1 + 0.001 * t2) ** 2
        0.5 + (t3 / t4)
    }

  def schaffer2[N <: Nat: GTEq2, A: Field: Trig](x: Dimension[N, A]) =
    x.pairs.mapSum {
      case (xi, xi1) =>
        val t1 = (xi ** 2) - (xi1 ** 2)
        val t2 = (xi ** 2) + (xi1 ** 2)
        val t3 = (sin((t1) ** 2) ** 2) - 0.5
        val t4 = (1 + 0.001 * t2) ** 2
        0.5 + (t3 / t4)
    }

  def schaffer3[N <: Nat: GTEq2, A: Field: Signed: Trig](x: Dimension[N, A]) =
    x.pairs.mapSum {
      case (xi, xi1) =>
        val t1 = cos(abs((xi ** 2) - (xi1 ** 2)))
        val t2 = (xi ** 2) + (xi1 ** 2)
        val t3 = (sin((t1) ** 2) ** 2) - 0.5
        val t4 = (1 + 0.001 * t2) ** 2
        0.5 + (t3 / t4)
    }

  def schaffer4[N <: Nat: GTEq2, A: Field: Trig](x: Dimension[N, A]) =
    x.pairs.mapSum {
      case (xi, xi1) =>
        val t1 = sin((xi ** 2) - (xi1 ** 2))
        val t2 = (xi ** 2) + (xi1 ** 2)
        val t3 = (cos((t1) ** 2) ** 2) - 0.5
        val t4 = (1 + 0.001 * t2) ** 2
        0.5 + (t3 / t4)
    }

  def schaffer6[N <: Nat: GTEq2, A: Field: NRoot: Trig](x: Dimension[N, A]) =
    x.toList.pairs.mapSum {
      case (xi, xi1) =>
        val t1 = (xi ** 2) + (xi1 ** 2)
        val t2 = (xi ** 2) + (xi1 ** 2)
        val t3 = (sin(sqrt(t1)) ** 2) - 0.5
        val t4 = (1 + 0.001 * t2) ** 2
        0.5 + (t3 / t4)
    }

  def schumerSteiglitz[N <: Nat, A: Field](x: Dimension[N, A]) =
    x.mapSum(_ ** 4)

  def schwefel1[N <: Nat, A: Field: NRoot](x: Dimension[N, A]) = {
    val α = sqrt(pi)
    x.mapSum(_ ** 2) ** α
  }

  def schwefel12[N <: Nat, A: Ring](x: Dimension[N, A]) =
    x.zipWithIndex.mapSum {
      case (xi, i) => x.toList.take(i + 1).mapSum(xi => xi) ** 2
    }

  def schwefel220[N <: Nat, A: Ring: Signed](x: Dimension[N, A]) =
    x.mapSum(abs(_))

  def schwefel221[N <: Nat: HasHead, A: Order: Signed](x: Dimension[N, A]) =
    x.toList.fold(abs(x.head)) { (xi, xi1) =>
      spire.math.max(abs(xi), abs(xi1))
    }

  def schwefel222[N <: Nat, A: Field: Signed](x: Dimension[N, A]) =
    x.mapSum(abs(_)) + x.mapProduct(abs(_))

  def schwefel223[N <: Nat, A: Ring](x: Dimension[N, A]) =
    x.mapSum(_ ** 10)

  def schwefel225[N <: Nat: HasHead, A: Field](x: Dimension[N, A]) =
    x.mapSum(xi => ((xi - 1) ** 2) + ((x.head - (xi ** 2)) ** 2))

  def schwefel226[N <: Nat, A: Field: NRoot: Signed: Trig](x: Dimension[N, A]) =
    418.9829 * x.size - x.mapSum(xi => xi * sin(sqrt(abs(xi))))

  def schwefel236[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    -x1 * x2 * (72 - 2 * x1 - 2 * x2)
  }

  def schwefel24[N <: Nat: HasHead, A: Field](x: Dimension[N, A]) =
    x.mapSum(xi => ((x.head - 1) ** 2) + ((x.head - xi) ** 2))

  def schwefel26[A: Order: Ring: Signed](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    spire.math.max(abs(x1 + 2 * x2 - 7), abs(2 * x1 + x2 - 5))
  }

  def shekel5[A: Field](x: Dimension4[A]) = {
    val a = Sized(
      Sized(4, 4, 4, 4),
      Sized(1, 1, 1, 1),
      Sized(8, 8, 8, 8),
      Sized(6, 6, 6, 6),
      Sized(3, 7, 3, 7)
    )
    val c = Sized(0.1, 0.2, 0.2, 0.4, 0.6)

    -(a zip c).mapSum {
      case (ai, ci) =>
        1 / (ci + (x zip ai).mapSum { case (xj, aij) => (xj - aij) ** 2 })
    }
  }

  def shekel7[A: Field](x: Dimension4[A]) = {
    val a = Sized(
      Sized(4, 4, 4, 4),
      Sized(1, 1, 1, 1),
      Sized(8, 8, 8, 8),
      Sized(6, 6, 6, 6),
      Sized(3, 7, 3, 7),
      Sized(2, 9, 2, 9),
      Sized(5, 5, 3, 3)
    )
    val c = Sized(0.1, 0.2, 0.2, 0.4, 0.4, 0.6, 0.3)

    -(a zip c).mapSum {
      case (ai, ci) =>
        1.0 / (ci + (x zip ai).mapSum { case (xj, aij) => (xj - aij) ** 2 })
    }
  }

  def shekel10[A: Field](x: Dimension4[A]) = {
    val a = Sized(
      Sized(4.0, 4.0, 4.0, 4.0),
      Sized(1.0, 1.0, 1.0, 1.0),
      Sized(8.0, 8.0, 8.0, 8.0),
      Sized(6.0, 6.0, 6.0, 6.0),
      Sized(3.0, 7.0, 3.0, 7.0),
      Sized(2.0, 9.0, 2.0, 9.0),
      Sized(5.0, 5.0, 3.0, 3.0),
      Sized(8.0, 1.0, 8.0, 1.0),
      Sized(6.0, 2.0, 6.0, 2.0),
      Sized(7.0, 3.6, 7.0, 3.6)
    )
    val c = Sized(0.1, 0.2, 0.2, 0.4, 0.4, 0.6, 0.3, 0.7, 0.5, 0.5)

    -(a zip c).mapSum {
      case (ai, ci) =>
        1.0 / (ci + (x zip ai).mapSum { case (xj, aij) => (xj - aij) ** 2 })
    }
  }

  def shubert[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    -x.mapProduct { xi =>
      (1 to 5).mapSum(j => j * cos((j + 1) * xi + j))
    }

  def shubert1[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = (1 to 5).mapSum(j => j * cos((j + 1) * x1 + j))
    val t2       = (1 to 5).mapSum(j => j * cos((j + 1) * x2 + j))
    t1 * t2
  }

  def shubert3[A: Ring: Trig](x: Dimension2[A]) =
    x.mapSum(xi => (1 to 5).mapSum(j => -j * sin((j + 1) * xi + j)))

  def shubert4[A: Ring: Trig](x: Dimension2[A]) =
    x.mapSum(xi => (1 to 5).mapSum(j => -j * cos((j + 1) * xi + j)))

  def sineEnvelope[A: Field: NRoot: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val numer    = (sin(sqrt((x1 ** 2) + (x2 ** 2))) ** 2) - 0.5
    val denom    = 1 + 0.0001 * (((x1 ** 2) + (x2 ** 2)) ** 2)
    0.5 + (numer / denom)
  }

  def sixHumpCamelback[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val tX1      = 4 * (x1 ** 2) - 2.1 * (x1 ** 4) + ((x1 ** 6) / 3.0)
    val tX2      = x1 * x2 - 4 * (x2 ** 2) + 4 * (x2 ** 4)
    tX1 + tX2
  }

  def spherical[N <: Nat, A: Ring](x: Dimension[N, A]) =
    x.mapSum(_ ** 2)

  def step1[N <: Nat, A: IsReal: Ring: Signed](x: Dimension[N, A]) =
    x.mapSum(xi => floor(abs(xi)))

  def step2[N <: Nat, A: Field: IsReal](x: Dimension[N, A]) =
    x.mapSum(xi => (floor(xi) + 0.5) ** 2)

  def step3[N <: Nat, A: IsReal: Ring](x: Dimension[N, A]) =
    x.mapSum(xi => floor(xi ** 2))

  def stretchedVSineWave[N <: Nat: GTEq2, A: Field: NRoot: Trig](x: Dimension[N, A]) =
    x.pairs.mapSum {
      case (xi, xi1) =>
        val t1 = ((xi1 ** 2) + (xi ** 2)) ** 0.25
        val t2 = sin(50 * (((xi1 ** 2) + (xi ** 2) ** 0.1))) ** 2 + 0.1
        t1 * t2
    }

  def styblinksiTang[N <: Nat, A: Field](x: Dimension[N, A]) =
    0.5 * x.mapSum(xi => (xi ** 4) - 16 * (xi ** 2) + 5 * xi)

  def sumSquares[N <: Nat, A: Ring](x: Dimension[N, A]) =
    x.zipWithIndex.mapSum { case (xi, i) => (i + 1) * (xi ** 2) }

  def sumDifferentPowers[N <: Nat, A: Ring: Signed](x: Dimension[N, A]) =
    x.zipWithIndex.mapSum { case (xi, i) => abs(xi) ** (i + 2) }

  def threeHumpCamelback[A: Field: IsReal](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    2 * (x1 ** 2) - 1.05 * (x1 ** 4) + ((x1 ** 6) / 6) + x1 * x2 + x2 ** 2
  }

  def trecanni[A: Ring](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    x1 ** 4 + 4 * (x1 ** 3) + 4 * (x1 ** 2) + (x2 ** 2)
  }

  def trefethen[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = 0.25 * (x1 ** 2) + 0.25 * (x2 ** 2)
    val t2       = exp(sin(50 * x1)) - sin(10 * (x1 + x2))
    val t3       = sin(60 * exp(x2))
    val t4       = sin(70 * sin(x1))
    val t5       = sin(sin(80 * x2))
    t1 + t2 + t3 + t4 + t5
  }

  def trid[N <: Nat: GTEq2, A: Field](x: Dimension[N, A]) = {
    val t1 = x.mapSum(xi => (xi - 1) ** 2)
    val t2 = x.pairs.mapSum { case (xi, xi1) => xi * xi1 }
    t1 - t2
  }

  def trigonometric1[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    x.zipWithIndex.mapSum {
      case (xi, i) =>
        (x.size - x.mapSum(cos(_)) + i * (1.0 - cos(xi) - sin(xi))) ** 2
    }

  def trigonometric2[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    1.0 + x.mapSum { xi =>
      val co = (xi - 0.9) ** 2
      val t1 = 8 * (sin(7 * co) ** 2)
      val t2 = 6 * (sin(14 * co) ** 2)
      t1 + t2 + co
    }

  def tripod[A: Order: Signed](x: Dimension2[A])(implicit A: Field[A]) = {
    val (x1, x2) = x.tuple
    def p(xi: A) = if (xi >= A.zero) A.one else A.zero

    val t1 = p(x2) * (1 + p(x2))
    val t2 = abs(x1 + 50 * p(x2) * (1 - 2 * p(x1)))
    val t3 = abs(x2 + 50 * (1 - 2 * p(x2)))
    t1 + t2 + t3
  }

  def unevenDecreasingMaxima[A: Field: NRoot: Trig](x: Dimension1[A]) = {
    val xi = x.head
    val t1 = exp(-2 * log(2) * (((xi - 0.08) / 0.854) ** 2))
    val t2 = sin(5 * pi * (xi.**(3.0 / 4.0) - 0.05)) ** 6
    t1 * t2
  }

  def ursem1[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    -sin(2 * x1 - 0.5 * pi) - 3 * cos(x2) - 0.5 * x1
  }

  def ursem3[A: Field: Trig: Signed](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val co1      = -sin(2.2 * pi * x1 + 0.5 * pi)
    val co2      = -sin(2.2 * pi * x2 + 0.5 * pi)
    val t1       = co1 * ((2 - abs(x1)) / (2)) * ((3 - abs(x1)) / (2))
    val t2       = co2 * ((2 - abs(x2)) / (2)) * ((3 - abs(x2)) / (2))
    t1 + t2
  }

  def ursem4[A: Field: NRoot: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = -3 * sin(0.5 * pi * x1 + 0.5 * pi)
    val t2       = (2 - sqrt((x1 ** 2) + (x2 ** 2))) / 4
    t1 * t2
  }

  def ursemWaves[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = -0.9 * (x1 ** 2)
    val t2       = ((x2 ** 2) - 4.5 * (x2 ** 2)) * x1 * x2
    val t3       = 4.7 * cos(3 * x1 - (x2 ** 2) * (2 + x1))
    val t4       = sin(2.5 * pi * x1)
    t1 + t2 + t3 * t4
  }

  def venterSobiezcczanskiSobieski[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = (x1 ** 2) - 100 * (cos(x1) ** 2)
    val t2       = -100 * cos((x1 ** 2) / 30) + (x2 ** 2)
    val t3       = -100 * (cos(x2) ** 2) - 100 * cos((x2 ** 2) / 30)
    t1 + t2 + t3
  }

  def vincent[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    -x.mapSum(xi => sin(10 * log(xi)))

  def watson[A: Field](x: Dimension6[A]) = {
    val x1 = x.head

    val t1 = (0 to 29).mapSum { i =>
      val ai = i / 29.0
      val sum4 = x.toList.tail.zipWithIndex.mapSum {
        case (xj, j) =>
          (j + 1) * (ai ** j.toDouble) * xj
      }
      val sum5 = x.zipWithIndex.mapSum {
        case (xk, k) =>
          (ai ** k.toDouble) * xk
      }
      (sum4 - (sum5 ** 2) - 1) ** 2
    }
    t1 + (x1 ** 2)
  }

  def wayburnSeader1[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = ((x1 ** 6) + (x2 ** 4) - 17) ** 2
    val t2       = (2 * x1 + x2 - 4) ** 2
    t1 + t2
  }

  def wavy[N <: Nat, A: Field: Trig](k: A = 10.0)(x: Dimension[N, A]) =
    1 - (x.mapSum { xi =>
      cos(k * xi) * exp(-(xi ** 2) / 2)
    } / x.size)

  def wayburnSeader2[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = (1.613 - 4 * ((x1 - 0.3125) ** 2) - 4 * ((x2 - 1.625) ** 2)) ** 2
    val t2       = (x2 - 1) ** 2
    t1 + t2
  }

  def wayburnSeader3[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    val t1       = 2 * ((x1 ** 3) / 3) - 8 * (x1 ** 2) + 33 * x1 - x1 * x2 + 5
    val t2       = (((x1 - 4) ** 2) + ((x2 - 5.0) ** 2) - 4) ** 2
    t1 + t2
  }

  def weierstrass[N <: Nat, A: Field: Trig](x: Dimension[N, A]) = {
    val a    = 0.5
    val b    = 3.0
    val kmax = 20
    val constant = (0 to kmax).toList.map { k =>
      val t1 = a ** k.toDouble
      val t2 = cos(pi * (b ** k.toDouble))
      t1 * t2
    }.sum

    val factor1 = x.mapSum { xi =>
      (0 to kmax).mapSum { k =>
        val t1 = a ** k.toDouble
        val t2 = cos(2 * pi * (b ** k.toDouble) * (xi + 0.5))
        t1 * t2
      }
    }

    factor1 - x.size * constant
  }

  def whitley[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    x.mapSum { xi =>
      x.mapSum { xj =>
        val factor = 100 * (((xi ** 2) - xj) ** 2) + ((1 - xj) ** 2)
        val t1     = (factor ** 2) / 4000
        val t2     = cos(factor)
        t1 - t2 + 1
      }
    }

  def wolfe[A: Field: NRoot](x: Dimension3[A]) = {
    val (x1, x2, x3) = x.tuple
    (4.0 / 3.0) * (((x1 ** 2) + (x2 ** 2)) ** 0.75) + x3
  }

  def wood[A: Field](x: Dimension4[A]) = {
    val (x1, x2, x3, x4) = x.tuple
    val t1               = 100 * (((x1 ** 2) - x2) ** 2)
    val t2               = (x1 - 1) ** 2 + (x3 - 1) ** 2
    val t3               = 90 * ((x3 ** 2 - x4) ** 2)
    val t4               = 10.1 * ((x2 - 1) ** 2)
    val t5               = (x4 - 1) ** 2 + 19.8 * (x2 - 1) * (x4 - 1)
    t1 + t2 + t3 + t4 + t5
  }

  def xinSheYang2[N <: Nat, A: Field: Signed: Trig](x: Dimension[N, A]) = {
    val t1 = x.mapSum(abs(_))
    val t2 = exp(-x.mapSum(xi => sin(xi ** 2)))
    t1 * t2
  }

  def xinSheYang3[N <: Nat, A: Field: NRoot: Trig](m: A = 5.0)(x: Dimension[N, A]) = {
    val β = 15.0
    val u = x.mapSum(xi => (xi / β).fpow(2 * m))
    val v = x.mapSum(_ ** 2)
    val w = x.mapProduct(xi => cos(xi) ** 2)
    exp(-u) - 2 * exp(-v) * w
  }

  def xinSheYang4[N <: Nat, A: Field: NRoot: Signed: Trig](x: Dimension[N, A]) = {
    val u = x.mapSum(xi => sin(xi) ** 2)
    val v = x.mapSum(_ ** 2)
    val w = x.mapSum(xi => sin(sqrt(abs(xi))) ** 2)
    (u - exp(-v)) * exp(-w)
  }

  def yaoLiu4[N <: Nat, A: Signed: Ordering](x: Dimension[N, A]) =
    x.map(abs(_)).max

  def yaoLiu9[N <: Nat, A: Field: Trig](x: Dimension[N, A]) =
    x.mapSum(xi => (xi ** 2) - 10 * cos(2 * pi * xi) + 10)

  def zakharov[N <: Nat, A: Field: IsReal](x: Dimension[N, A]) = {
    val t = x.zipWithIndex.mapSum { case (xi, i) => 0.5 * i * xi }
    spherical(x) + (t ** 2) + (t ** 4)
  }

  def zeroSum[N <: Nat, A: Signed: NRoot](x: Dimension[N, A])(implicit A: Field[A]) = {
    val sum = x.mapSum(xi => xi)
    if (sum == 0.0) A.zero
    else 1 + ((10000 * abs(sum)) ** 0.5)
  }

  def zettle[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    (x1 ** 2 + x2 ** 2 - 2 * x1) ** 2 + x1 / 4
  }

  def zirilli1[A: Field](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    0.25 * (x1 ** 4) - 0.5 * (x1 ** 2) + 0.1 * x1 + 0.5 * (x2 ** 2)
  }

  def zirilli2[A: Field: Trig](x: Dimension2[A]) = {
    val (x1, x2) = x.tuple
    0.5 * (x1 ** 2) + 0.5 * (1.0 - cos(2 * x1)) + (x2 ** 2)
  }

}
