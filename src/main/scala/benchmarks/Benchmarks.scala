package benchmarks

import zio.prelude.{NonEmptyList, ForEach}

import scala.math._

object Benchmarks {

  def impossible = sys.error("This case is not possible but included to keep the compiler happy")

  def mapSum[F[+_]: ForEach, A, B](x: F[A])(f: A => B)(implicit ev: Numeric[B]): B =
    ForEach[F].foldLeft(x)(ev.zero)((b: B, a: A) => ev.plus(b, f(a)))

  def mapProduct[F[+_]: ForEach, A, B](x: F[A])(f: A => B)(implicit ev: Numeric[B]): B =
    ForEach[F].foldLeft(x)(ev.one)((b: B, a: A) => ev.times(b, f(a)))

  def pairs[F[+_]: ForEach, A](x: F[A]) =
    ForEach[F].toList(x).sliding(2).map { case Seq(x1, x2) => (x1, x2) }.toList

  val square = (x: Double) => x * x


  def absoluteValue(x: NonEmptyList[Double]): Double =
    mapSum(x)(abs(_))

  def ackley(x: NonEmptyList[Double]) = {
    val n      = x.size
    val sumcos = mapSum(x)(xi => cos(2 * Pi * xi))
    val sumsqr = mapSum(x)(a => a * a)

    -20 * exp(-0.2 * sqrt(sumsqr / n)) - exp(sumcos / n) + 20 + E
  }

  def ackley2(x: (Double, Double)) = {
    val (x1, x2) = x
    -200 * exp(-0.02 * sqrt((x1 *x1) + (x2 * x2)))
  }

  def ackley3(x: (Double, Double)) = {
    val (x1, x2) = x
    ackley2(x) + 5 * exp(cos(3 * x1) + sin(3 * x2))
  }

  def adjiman(x: (Double, Double)) = {
    val (x1, x2) = x
    cos(x1) * sin(x2) - (x1) / (x2 * x2 + 1)
  }

  def alpine1(x: NonEmptyList[Double]) =
    mapSum(x)(xi => abs((xi * sin(xi)) + (0.1 * xi)))

  def alpine2(x: NonEmptyList[Double]) =
    mapProduct(x)(xi => sqrt(xi) * sin(xi))

  def arithmeticMean(x: NonEmptyList[Double]) = {
    val n        = x.size
    val avg      = mapSum(x)(xi => xi) / n
    val rootProd = pow(mapProduct(x)(xi => xi), 1.0 / n)

    val result= avg - rootProd

    result * result
  }

  def bartelsConn(x: (Double, Double)) = {
    val (x1, x2) = x
    abs(x1 * x1 + x2 * x2 + x1 * x2) + abs(sin(x1)) + abs(cos(x2))
  }

  def beale(x: (Double, Double)) = {
    val (x1, x2) = x

    val a = (1.5 - x1 + x1 * x2)
    val b = (2.25 - x1 + x1 * (x2 * x2))
    val c = (2.625 - x1 + x1 * (x2 * x2 * x2))

    a*a + b*b + c*c
  }

  def biggsEXP2(x: (Double, Double)) = {
    val (x1, x2) = x
    mapSum(1 to 10) { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5 * exp(-10 * ti)
      val result = (exp(-ti * x1) - 5 * exp(-ti * x2) - yi)

      result * result
    }
  }

  def biggsEXP3(x: (Double, Double, Double)) = {
    val (x1, x2, x3) = x
    mapSum(1 to 10) { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5 * exp(-10 * ti)
      val result = (exp(-ti * x1) - x3 * exp(-ti * x2) - yi)
      result * result
    }
  }

  def biggsEXP4(x: (Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4) = x
    mapSum(1 to 10) { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5 * exp(-10 * ti)
      val r = (x3 * exp(-ti * x1) - x4 * exp(-ti * x2) - yi)
      r * r
    }
  }

  def biggsEXP5(x: (Double, Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4, x5) = x
    mapSum(1 to 11) { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5 * exp(-10 * ti) + 3 * exp(-4 * ti)
      val r = (x3 * exp(-ti * x1) - x4 * exp(-ti * x2) + 3 * exp(-ti * x5) - yi)
      r * r
    }
  }

  def biggsEXP6(x: (Double, Double, Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4, x5, x6) = x
    mapSum(1 to 13) { i =>
      val ti = 0.1 * i
      val yi = exp(-ti) - 5 * exp(-10 * ti) + 3 * exp(-4 * ti)
      val r = (x3 * exp(-ti * x1) - x4 * exp(-ti * x2) + x6 * exp(-ti * x5) - yi)
      r * r
    }
  }

  def bird(x: (Double, Double)) = {
    val (x1, x2) = x
    sin(x1) * exp(pow(1 - cos(x2), 2)) +
      cos(x2) * exp(pow(1 - sin(x1), 2)) + pow(x1 - x2, 2)
  }

  def bohachevsky1(x: (Double, Double)) = {
    val (x1, x2) = x
    (x1 * x1) + 2 * (x2 * x2) - 0.3 *
      cos(3 * Pi * x1) - 0.4 * cos(4 * Pi * x2) + 0.7
  }

  def bohachevsky2(x: (Double, Double)) = {
    val (x1, x2) = x
    (x1 * x1) + 2 * (x2 * x2) - 0.3 *
      cos(3 * Pi * x1) * cos(4 * Pi * x2) + 0.3
  }

  def bohachevsky3(x: (Double, Double)) = {
    val (x1, x2) = x
    (x1 * x1) + 2 * (x2 * x2) - 0.3 *
      cos(3 * Pi * x1 + 4 * Pi * x2) + 0.3
  }

  def booth(x: (Double, Double)) = {
    val (x1, x2) = x
    val a = (x1 + 2 * x2 - 7)
    val b = (2 * x1 + x2 - 5)
    a*a + b*b
  }

  def boxBettsQuadraticSum(k: Int)(x: (Double, Double, Double)) = {
    val (x1, x2, x3) = x
    mapSum(1 to k) { i =>
      val co = -0.1 * i
      val t1 = exp(co * x1)
      val t2 = exp(co * x2)
      val t3 = (exp(co) - exp(-i.toDouble)) * x3
      val r = (t1 - t2 - t3)
      r * r
    }
  }

  def brad(x: (Double, Double, Double)) = {
    val (x1, x2, x3) = x
    val y = List(
      0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39
    )

    mapSum(y.zipWithIndex) {
      case (yi, i) =>
        val ui = i + 1
        val vi = 16 - ui
        val wi = min(ui, vi)
        val r = ((yi - x1 - ui) / (vi * x2 + wi * x3))
        r * r
    }
  }

  def braninRCOS1(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1 = (x2 - (5.1 / (4 * (Pi*Pi))) * (x1 * x1) + (5 / Pi) * x1 - 6)
    val t2 = 10 * (1 - 1 / (8 * Pi)) * cos(x1)
    t1*t1 + t2 + 10
  }

  def braninRCOS2(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = (-1.275 * (x1 * x1) / (Pi * Pi) + (5 * x1) / Pi + x2 - 6)
    val t2       = (10 - 5 / (4 * Pi)) * cos(x1) * cos(x2)
    val t3       = log((x1 * x1) + (x2 * x2) + 1) + 10
    t1*t1 + t2 + t3
  }

  def brent(x: NonEmptyList[Double]) =
    mapSum(x)(xi => (xi + 10) * (xi + 10)) + exp(mapSum(x.map(_ * -1))(xi => xi*xi))

  def brown(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x).toChunk.toList)) {
      case (xi, xi1) => pow(xi * xi, (xi1 * xi1) + 1) + pow(xi1 * xi1, (xi * xi) + 1)
    }

  def bukin2(x: (Double, Double)) = {
    val (x1, x2) = x
    100 * pow(x2 - 0.01 * (x1 * x1) + 1, 2) + 0.01 * pow(x1 + 10, 2)
  }

  def bukin2Adapted(x: (Double, Double)) = {
    val r = bukin2(x)
    r * r
  }

  def bukin4(x: (Double, Double)) = {
    val (x1, x2) = x
    100 * (x2 * x2) + 0.01 * abs(x1 + 10)
  }

  def bukin6(x: (Double, Double)) = {
    val (x1, x2) = x
    100 * sqrt(abs(x2 - 0.01 * (x1 * x1))) + 0.01 * abs(x1 + 10)
  }

  def carromTable(x: (Double, Double)) = {
    val (x1, x2) = x
    val u        = cos(x1) * cos(x2)
    val v        = sqrt((x1 * x1) + (x2 * x2))
    -(pow(u * exp(abs(1 - v / Pi)), 2)) / 30.0
  }

  def centralTwoPeakTrap(x1: Double) =
    if (x1 < 0) 0.0
    else if (x1 <= 10) (-160.0 / 10) * x1
    else if (x1 <= 15) (-160.0 / 5) * (15 - x1)
    else if (x1 <= 20) (-200.0 / 5) * (x1 - 15)
    else -200.0

  def chichinadze(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = (x1 * x1) - (12 * x1) + 11
    val t2       = 10 * cos(Pi * (x1 / 2)) + 8 * sin(5 * Pi * x1)
    val t3       = sqrt(1.0 / 5) * exp(-0.5 * ((x2 - 0.5) * (x2 - 0.5)))
    t1 + t2 - t3
  }

  def chungReynolds(x: NonEmptyList[Double]) = {
    val r = mapSum(x)(square)
    r * r
  }

  def cigar(condition: Double = 10e6)(x: AtLeast2List) =
    AtLeast2List.unwrap(x).toChunk.toList match {
      case x1 :: x2 :: rest =>
        x1*x1 + x2*x2 * condition + mapSum(rest)(square) * condition

      case _ => impossible
    }

  def colville(x: (Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4) = x
    val t1               = 100 * pow((x1 - (x2*x2)), 2) + pow((1 - x1), 2)
    val t2               = 90 * pow(x4 - x3, 2) + pow((1 - x3), 2)
    val t3               = 10.1 * (pow((x2 - 1), 2) + pow((x4 - 1), 2))
    val t4               = 19.8 * (x2 - 1) * (x4 - 1)
    t1 + t2 + t3 + t4
  }

  def corana(a: Double = 0.05)(x: (Double, Double, Double, Double)) = {
    val d = List(1, 1000, 10, 100)

    mapSum(List(x._1, x._2, x._3, x._4) zip d) {
      case (xi, di) =>
        val zi = 0.2 * floor(abs(xi / 0.2) + 0.49999) * signum(xi)
        val vi = abs(xi - zi)

        if (abs(vi) < a) //implicitly[Order[A]].lt(abs(vi), a))
          0.15 * pow((zi - a * (signum(zi))), 2) * di
        else
          di * (xi * xi)
    }
  }

  def cosineMixture(x: NonEmptyList[Double]) =
    -0.1 * mapSum(x)(xi => cos(5 * Pi * xi)) + mapSum(x)(square)

  def crossInTray(x: NonEmptyList[Double]) = {
    val t1 = mapProduct(x)(xi => sin(xi))
    val t2 = exp(abs(100 - (sqrt(mapSum(x)(square)) / Pi)))
    -0.0001 * pow((abs(t1 * t2) + 1), 0.1)
  }

  def crossLegTable(x: NonEmptyList[Double]) =
    -1 / (crossInTray(x) / -0.0001)

  def crossCrowned(x: NonEmptyList[Double]) =
    -crossInTray(x)

  def csendes(x: NonEmptyList[Double]) =
    if (x.exists(_ == 0.0)) None
    else Some(mapSum(x)(xi => pow(xi, 6) * (2 + sin(1 / xi))))

  def cube(x: (Double, Double)) = {
    val (x1, x2) = x
    100 * pow((x2 - (x1 * x1 * x1)), 2) + pow((1 - x1), 2)
  }

  def damavandi(x: (Double, Double)) = {
    val (x1, x2) = x
    if ((x1 != 2.0) && (x2 != 2.0)) {
      val numer   = sin(Pi * (x1 - 2)) * sin(Pi * (x2 - 2))
      val denom   = (Pi * Pi) * (x1 - 2) * (x2 - 2)
      val factor1 = 1 - pow(abs(numer / denom), 5)
      val factor2 = 2 + pow((x1 - 7), 2) + 2 * pow((x2 - 7), 2)
      Some(factor1 * factor2)
    } else None
  }

  def deb1(x: NonEmptyList[Double]) =
    -(1.0 / x.size) * mapSum(x)(xi => pow(sin(5 * Pi * xi), 6))

  def deb2(x: NonEmptyList[Double]) =
    deb1(NonEmptyList(mapSum(x)(xi => pow(xi, 0.75) - 0.05)))

  def decanomial(x: (Double, Double)) = {
    val (x1, x2) = x
    val coX1     = List(1, -20, 180, -960, 3360, -8064, 13340, -15360, 11520, -5120, 2624)
    val coX2     = List(1, 12, 54, 108, 81)

    def one(l: List[Int], xi: Double) =
      abs(mapSum(l.zipWithIndex) {
        case (ci, i) => ci * pow(xi, (l.size - 1.0 - i))
      })

    0.001 * (pow(one(coX2, x2) + one(coX1, x1), 2))
  }

  def deckkersAarts(x: (Double, Double)) = {
    val x1 = x._1 * x._1
    val x2 = x._2 * x._2
    val t1       = 100000 * x1 + x2
    val t2       = (x1 + x2) * (x1 + x2)
    val t3       = (1.0 / 100000) * (pow(x1 + x2, 4))

    t1 - t2 + t3
  }

  def deflectedCorrugatedSpring(K: Double = 5.0)(x: NonEmptyList[Double]) = {
    val α     = 5
    val inner = mapSum(x)(xi => pow(xi - α, 2))
    val outer = mapSum(x)(xi => pow(xi - α, 2) - cos(K * sqrt(inner)))
    0.1 * outer
  }

  def deVilliersGlasser1(x: (Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4) = x
    mapSum(1 to 24) { i =>
      val ti = 0.1 * (i - 1)
      val yi = 60.137 * pow(1.371, ti) * sin(3.112 * ti + 1.761)
      val t1 = x1 * pow(x2, ti)
      val t2 = sin(x3 * ti + x4)
      val t3 = yi
      val r = t1 * t2 - t3
      r * r
    }
  }

  def deVilliersGlasser2(x: (Double, Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4, x5) = x
    mapSum(1 to 16) { i =>
      val ti = 0.1 * (i - 1)
      val yi = 53.81 * pow(1.27, ti) * tanh(3.012 * ti + sin(2.13 * ti)) *
        cos(exp(0.507) * ti)
      val t1 = x1 * pow(x2, ti)
      val t2 = tanh(x3 * ti + sin(x4 * ti))
      val t3 = cos(ti * exp(x5))
      val t4 = yi

      val r = t1 * t2 * t3 - t4

      r * r
    }
  }

  def differentPowers(x: AtLeast2List) = {
    val l = AtLeast2List.unwrap(x).toChunk.toList
    val n = l.size
    val inner = mapSum(l.zipWithIndex) {
      case (xi, i) => pow(abs(xi), (2.0 + ((4 * i) / (n - 1))))
    }
    sqrt(inner)
  }

  def discus(x: NonEmptyList[Double]) =
    pow(10, 6) * (x.head * x.head) + mapSum(x.tail)(square)

  def dixonPrice(x: AtLeast2List) = {
    val l = AtLeast2List.unwrap(x)
    val t1 = pow(l.head - 1, 2)
    val t2 = mapSum(pairs(l).zipWithIndex) {
      case ((xi, xi1), i) => (i + 2) * pow((2 * (xi1 * xi1)) - xi, 2)
    }
    t1 + t2
  }

  def dolan(x: (Double, Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4, x5) = x
    val t1                   = (x1 + 1.7 * x2) * sin(x1)
    val t2                   = -1.5 * x3 - 0.1 * x4 * cos(x4 + x5 - x1)
    val t3                   = 0.2 * (x5 * x5) - x2 - 1
    abs(t1 + t2 + t3)
  }

  def dropWave(x: NonEmptyList[Double]) = {
    val sumsqr = mapSum(x)(square)
    -(1 + cos(12 * sqrt(sumsqr))) / (2 + 0.5 * sumsqr)
  }

  def easom(x: (Double, Double)) = {
    val (x1, x2) = x
    -cos(x1) * cos(x2) * exp(-(pow(x1 - Pi, 2) + pow(x2 - Pi, 2)))
  }

  def eggCrate(x: NonEmptyList[Double]) =
    mapSum(x)(square) + 24 * mapSum(x)(xi => sin(xi) * sin(xi))

  def eggHolder(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (x1, x2) =>
        -(x2 + 47) * sin(sqrt(abs(x2 + (x1 / 2) + 47))) - x1 * sin(sqrt(abs(x1 - x2 - 47)))
    }

  def elAttarVidyasagarDutta(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = (x1 * x1) + x2 - 10
    val t2       = x1 + (x2 * x2) - 7
    val t3       = (x1 * x1) + (x2 * x2 * x2) - 1
    t1*t1 + t2*t2 + t3*t3
  }

  def elliptic(x: AtLeast2List) = {
    val l = AtLeast2List.unwrap(x)
    val n = l.length
    mapSum(l.zipWithIndex) {
      case (xi, i) => pow(1e6, (i / (n - 1.0))) * (xi * xi)
    }
  }

  def equalMaxima(x: Double) =
    pow(sin(5 * Pi * x), 6)

  def exponential1(x: NonEmptyList[Double]) =
    -exp(-0.5 * mapSum(x)(square))

  def exponential2(x: (Double, Double)) = {
    val (x1, x2) = x
    mapSum(0 to 9) { i =>
      val t1 = 1 * exp(-i * x1 / 10)
      val t2 = 5 * exp(-i * x2 / 10)
      val t3 = 1 * exp(-i / 10.0)
      val t4 = 5 * exp(-i.toDouble)
      val r = t1 - t2 - t3 + t4
      r * r
    }
  }

  def fiveUnevenPeakTrap(x: Double) =
    x match {
      case xi if xi - 2.5 < 0  => 80 * (2.5 - xi)
      case xi if xi - 5.0 < 0  => 64 * (xi - 2.5)
      case xi if xi - 7.5 < 0  => 64 * (7.5 - xi)
      case xi if xi - 12.5 < 0 => 28 * (xi - 7.5)
      case xi if xi - 17.5 < 0 => 28 * (17.5 - xi)
      case xi if xi - 22.5 < 0 => 32 * (xi - 17.5)
      case xi if xi - 27.5 < 0 => 32 * (27.5 - xi)
      case xi                  => 80 * (xi - 27.5)
    }

  def freudensteinRoth(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = (x1 - 13 + ((5 - x2) * x2 - 2) * x2)
    val t2       = (x1 - 29 + ((x2 + 1) * x2 - 14) * x2)
    t1*t1 + t2*t2
  }

  def gear(x: (Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4) = x
    val t1               = 1 / 6.931
    val numer            = floor(x1) * floor(x2)
    val denom            = floor(x3) * floor(x4)
    val r = t1 - (numer / denom)
    r * r
  }

  def giunta(x: (Double, Double)) =
    0.6 + mapSum(List(x._1, x._2)) { xi =>
      val factor = (16.0 / 15.0) * xi - 1
      val t1     = sin(factor)
      val t2     = t1 * t1
      val t3     = (1.0 / 50.0) * sin(4 * factor)
      t1 + t2 + t3
    }

  def goldsteinPrice1(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1 = 1 + (pow(x1 + x2 + 1, 2)) * (19 - 14 * x1 + 3 * (x1 * x1) -
      14 * x2 + 6 * x1 * x2 + 3 * (x2 * x2))
    val t2 = 30 + (pow(2 * x1 - 3 * x2, 2)) * (18 - 32 * x1 + 12 *
      (x1 * x1) + 48 * x2 - 36 * x1 * x2 + 27 * (x2 * x2))
    t1 * t2
  }

  def goldsteinPrice2(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = exp(0.5 * pow(((x1 * x1) + (x2 * x2) - 25), 2))
    val t2       = pow(sin(4 * x1 - 3 * x2), 4)
    val t3       = 0.5 * (pow(2 * x1 + x2 - 10, 2))
    t1 + t2 + t3
  }

  def griewank(x: NonEmptyList[Double]) = {
    val prod = mapProduct(x.toList.zipWithIndex) {
      case (xi, i) =>
        cos(xi / sqrt(i + 1.0))
    }

    1 + mapSum(x)(square) / 4000.0 - prod
  }

  def gulf(x: (Double, Double, Double)) = {
    val (x1, x2, x3) = x

    mapSum(1 to 99) { i =>
      val ui    = 25 + pow(-50 * log(0.01 * i), 2.0 / 3.0)
      val numer = pow(abs(ui - x2), x3)
      val r = exp(-numer / x1) - (i / 100.0)
      r * r
    }
  }

  def hansen(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = mapSum(0 to 4)(i => (i + 1) * cos(i * x1 + i + 1))
    val t2       = mapSum(0 to 4)(j => (j + 1) * cos((j + 2) * x2 + j + 1))
    t1 * t2
  }

  def hartman3(x: (Double, Double, Double)) = {
    val xList = List(x._1, x._2, x._3)

    val a = List(
      List(3.0, 10.0, 30.0),
      List(0.1, 10.0, 35.0),
      List(3.0, 10.0, 30.0),
      List(0.1, 10.0, 35.0)
    )

    val c = List(1.0, 1.2, 3.0, 3.2)

    val p = List(
      List(0.6890, 0.1170, 0.2673),
      List(0.4699, 0.4387, 0.7470),
      List(0.1091, 0.8732, 0.5547),
      List(0.0381, 0.5743, 0.8828)
    )

    -mapSum(a.zip(c).zip(p)) {
      case ((ai, ci), pi) =>
        val power =  mapSum(ai zip pi zip xList) {
          case ((aij, pij), xj) =>
            aij * pow((xj - pij), 2)
        }
        ci * exp(-power)
    }
  }

  def hartman6(x: (Double, Double, Double, Double, Double, Double)) = {
    val xList = List(x._1, x._2, x._3, x._4, x._5, x._6)
    val a = List(
      List(10.0, 3.00, 17.0, 3.50, 1.70, 8.00),
      List(0.05, 10.0, 17.0, 0.10, 8.00, 14.0),
      List(3.00, 3.50, 1.70, 10.0, 17.0, 8.00),
      List(17.0, 8.00, 0.05, 10.0, 0.10, 14.0)
    )

    val c = List(1.0, 1.2, 3.0, 3.2)

    val p = List(
      List(0.1312, 0.1696, 0.5569, 0.0124, 0.8283, 0.5886),
      List(0.2329, 0.4135, 0.8307, 0.3736, 0.1004, 0.9991),
      List(0.2348, 0.1451, 0.3522, 0.2883, 0.3047, 0.6650),
      List(0.4047, 0.8828, 0.8732, 0.5743, 0.1091, 0.0381)
    )

    -mapSum(a zip c zip p) {
      case ((ai, ci), pi) =>
        val power = mapSum(ai zip pi zip xList) {
          case ((aij, pij), xj) =>
            aij * pow((xj - pij), 2)
        }
        ci * exp(-power)
    }
  }

  def helicalValley(x: (Double, Double, Double)) = {
    val (x1, x2, x3) = x
    val r            = sqrt((x1 * x1) + (x2 * x2))
    val θ            = 1 / (2 * Pi) * atan2(x2, x1)
    (x3 * x3) + 100 * (pow(x3 - 10 * θ, 2) + pow(r - 1, 2))
  }

  def himmelblau(x: (Double, Double)) = {
    val (x1, x2) = x
    pow(x1 * x1 + x2 - 11, 2) + pow(x1 + x2 * x2 - 7, 2)
  }

  def hosaki(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = 1 - 8 * x1 + 7 * (x1 * x1)
    val t2       = (7.0 / 3.0) * (x1 * x1 * x1)
    val t3       = (1.0 / 4.0) * (x1 * x1 * x1 * x1)
    val t4       = (x2 * x2) * exp(-x2)
    (t1 - t2 + t3) * t4
  }

  def hyperEllipsoid(x: NonEmptyList[Double]) =
    mapSum(x.zipWithIndex) { case (xi, i) => i * (xi * xi) }

  def hyperEllipsoidRotated(x: NonEmptyList[Double]) = {
    //val y      = x.toList
    val values = (1 to x.size).toList.map(x take _)
    mapSum(values)(a => mapSum(a)(square))
  }

  def jennrichSampson(x: (Double, Double)) = {
    val (x1, x2) = x
    mapSum(1 to 10) { i =>
      val t1 = 2 + 2 * i
      val t2 = exp(i * x1) + exp(i * x2)
      val r = (t1 - t2)
      r * r
    }
  }

  def judge(x: (Double, Double)) = {
    val (x1, x2) = x
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
    val mappedC = C.map(_ * (x2 * x2))

    val t1 = (mappedB zip mappedC).map { case (ai, bi) => ai + bi }
    val t2 = t1.map(_ + x1)

    mapSum(t2 zip A) {
      case (t2, ai) => (t2 - ai) * (t2 - ai)
    }
  }

  def katsuura(x: NonEmptyList[Double]) =
    mapProduct(x.zipWithIndex) {
      case (xi, i) =>
        val t1 = i + 1
        val d  = 32
        val t2 = mapSum(1 to d)(k => floor((2 << k) * xi) * (1.0 / (2 << k)))
        1 + t1 * t2
    }

  def keane(x: (Double, Double)) = {
    val (x1, x2) = x
    val numer    = pow(sin(x1 - x2), 2) * pow(sin(x1 + x2), 2)
    val denom    = sqrt((x1 * x1) + (x2 * x2))
    numer / denom
  }

  def kowalik(x: (Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4) = x
    val b = List(
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
    val a = List(
      0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 0.0323, 0.0235, 0.0246
    )
    mapSum(a zip b) {
      case (ai, bi) =>
        val numer = x1 * ((bi * bi) + bi * x2)
        val denom = (bi * bi) + bi * x3 + x4
        val r = (ai - (numer / denom))
        r * r
    }
  }

  def langermann(x: (Double, Double)) = {
    val (x1, x2) = x
    val a        = List(3, 5, 2, 1, 7)
    val b        = List(5, 2, 1, 4, 9)
    val c        = List(1, 2, 5, 2, 3)

    -mapSum(a zip b zip c) {
      case ((ai, bi), ci) =>
        val t1    = square(x1 - ai)
        val t2    = square(x2 - bi)
        val numer = ci * cos(Pi * (t1 + t2))
        val denom = exp((t1 + t2) / Pi)
        numer / denom
    }
  }

  def leon(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = 100 * pow(x2 - (x1 * x1), 2)
    val t2       = (1 - x1)
    t1 + t2*t2
  }

  def levy3(x: AtLeast2List) = {
    val list = AtLeast2List.unwrap(x)
    def y(xi: Double) = 1 + (xi - 1) / 4.0
    val t1          = pow(sin(Pi * y(list.head)), 2)
    val t2 = mapSum(pairs(list)) {
      case (xi, xi1) =>
        pow(y(xi) - 1, 2) * (1 + 10 * (Pi * pow(y(xi1), 2)))
    }
    val t3 = (y(list.last) - 1)
    t1 + t2 + t3*t3
  }

  def levy5(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = mapSum(1 to 5)(i => i * cos((i - 1) * x1 + i))
    val t2       = mapSum(1 to 5)(j => j * cos((j + 1) * x2 + j))
    val t3       = (x1 + 1.42513)
    val t4       = (x2 + 0.80032)
    t1 * t2 + t3*t3 + t4*t4
  }

  def levy13(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = ((x1 - 1) * (x1 - 1)) * (pow(sin(3 * Pi * x2), 2) + 1)
    val t2       = ((x2 - 1) * (x2 - 1)) * (pow(sin(2 * Pi * x2), 2) + 1)
    val t3       = pow(sin(3 * Pi * x1), 2)
    t1 + t2 + t3
  }

  def levyMontalvo2(x: AtLeast2List) = {
    val l = AtLeast2List.unwrap(x)
    val last = l.last
    val t1 = pow(sin(3 * Pi * l.head), 2)
    val t2 = mapSum(pairs(l)) {
      case (xi, xi1) =>
        ((xi - 1) * (xi - 1)) * (pow(sin(3 * Pi * xi1), 2) + 1)
    }
    val t3 = ((last - 1) * (last - 1)) * (pow(sin(2 * Pi * last), 2) + 1)
    0.1 * (t1 + t2 + t3)
  }

  def matyas(x: (Double, Double)) = {
    val (x1, x2) = x
    0.26 * (x1 * x1 + x2 * x2) - 0.48 * x1 * x2
  }

  def maximum(x: NonEmptyList[Double]) =
    x.max

  def mcCormick(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = sin(x1 + x2) + ((x1 - x2) * (x1 - x2))
    val t2       = -1.5 * x1 + 2.5 * x2 + 1
    t1 + t2
  }

  def michalewicz(m: Double = 10.0)(x: NonEmptyList[Double]) =
    -mapSum(x.zipWithIndex) {
      case (xi, i) =>
        sin(xi) * pow(sin((i + 1) * (xi * xi)) / Pi, 2 * m)
    }

  def mieleCantrell(x: (Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4) = x
    val t1               = pow(exp(-x1) - x2, 4)
    val t2               = 100 * pow(x2 - x3, 6)
    val t3               = pow(tan(x3 - x4), 4)
    val t4               = pow(x1, 8)
    t1 + t2 + t3 + t4
  }

  def minimum(x: NonEmptyList[Double]) =
    x.min

  def mishra1(x: NonEmptyList[Double]) = {
    val sum = mapSum(x.init)(xi => xi)
    val n   = x.size
    pow(1 + n - sum, n - sum)
  }

  def mishra2(x: AtLeast2List) = {
    val l = AtLeast2List.unwrap(x)
    val sum = mapSum(pairs(l)) {
      case (xi, xi1) => 0.5 * (xi + xi1)
    }
    val n = l.length
    pow(1 + n - sum, n - sum)
  }

  def mishra3(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = 0.01 * (x1 + x2)
    val t2       = sqrt(abs(cos(sqrt(abs((x1 * x1) + (x2 * x2))))))
    t1 + t2
  }

  def mishra4(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = 0.01 * (x1 + x2)
    val t2       = sqrt(abs(sin(sqrt(abs((x1 * x1) + (x2 * x2))))))
    t1 + t2
  }

  def mishra5(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = sin(pow(cos(x1) + cos(x2), 2))
    val t2       = cos(pow(sin(x1) + sin(x2), 2))
    val t3       = 0.01 * (x1 + x2)
    val r = t1*t1 + t2*t2 + x1

    r * r  + t3
  }

  def mishra6(x: (Double, Double)) = {
    val (x1, x2) = x
    val a        = 0.1 * ((x1 - 1) * (x1 - 1) + (x2 - 1) * (x2 -1))
    val u        = pow(cos(x1) + cos(x2), 2)
    val v        = pow(sin(x1) + sin(x2), 2)
    a - log(pow(sin(u) * sin(u) - cos(v) * cos(v) + x1, 2))
  }

  def mishra7(x: NonEmptyList[Double]) = {
    def factorial(n: Int, accu: Int): Int = n match {
      case 0 => accu
      case _ => factorial(n - 1, n * accu)
    }
    val n    = x.size
    val prod = mapProduct(x)(xi => xi)
    square(prod - factorial(n, 1))
  }

  def mishra8(x: (Double, Double)) = {
    val (x1, x2) = x
    val coX1     = List(1, -20, 180, -960, 3360, -8064, 13340, -15360, 11520, -5120, 2624)
    val coX2     = List(1, 12, 54, 108, 81)

    val t1 = abs(mapSum(coX1.zipWithIndex) {
      case (ci, i) => ci * (pow(x1, coX1.length - 1.0 - i))
    })

    val t2 = abs(mapSum(coX2.zipWithIndex) {
      case (ci, i) => ci * (pow(x2, coX2.length - 1.0 - i))
    })

    val r = t1 * t2

    0.001 * r * r
  }

  def mishra9(x: (Double, Double, Double)) = {
    val (x1, x2, x3) = x
    val a            = (2 * x1 * x1 * x1 + 5 * x1 * x2 + 4 * x3 - 2 * x1 * x1 * x3 - 18)
    val b            = x1 + x2 * x2 * x2 + x1 * x2 * x2 + x1 * x3 * x3 - 22
    val c            = (8 * x1 * x1 + 2 * x2 * x3 + 2 * x2 * x2 + 3 * x2 * x2 * x2 - 52)
    val d = x1 + x2 - x3
    val r = (a * (b * b) * c) + (a * b * (c * c)) + (b * b) + ( d * d)

    r * r
  }

  def mishra10(x: (Double, Double)) = {
    val (x1, x2) = x
    val f1       = floor(x1) + floor(x2)
    val f2       = floor(x1) * floor(x2)
    val r = f1 - f2
    r * r
  }

  def misha11(x: NonEmptyList[Double]) = {
    val n  = x.size
    val t1 = (1.0 / n) * mapSum(x)(abs(_))
    val t2 = pow(mapProduct(x)(abs(_)), 1.0 / n)
    val r = t1 - t2
    r * r
  }

  def multiModal(x: NonEmptyList[Double]) =
    mapProduct(x)(abs(_)) * mapSum(x)(abs(_))

  def needleEye(eye: Double = 0.0001)(x: NonEmptyList[Double])=
    if (x.forall(xi => abs(xi) < eye)) 1.0
    else if (x.forall(xi => abs(xi) > eye)) mapSum(x)(xi => 100 + abs(xi))
    else 0.0

  def newFunction1(x: (Double, Double)) = {
    val (x1, x2) = x
    sqrt(abs(cos(sqrt(abs((x1 * x1) + x2))))) + 0.01 * (x1 + x2)
  }

  def newFunction2(x: (Double, Double)) = {
    val (x1, x2) = x
    sqrt(abs(sin(sqrt(abs((x1 * x1) + x2))))) + 0.01 * (x1 + x2)
  }

  def norwegian(x: NonEmptyList[Double]) =
    mapProduct(x)(xi => cos(Pi * (xi * xi * xi)) * ((99 + xi) / (100)))

  def parsopoulus(x: (Double, Double)) = {
    val (x1, x2) = x
    (cos(x1) * cos(x1)) + (sin(x2) * sin(x2)) // trig identity????
  }

  def pathological(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (xi, xi1) =>
        val numer = pow(sin(sqrt(100 * (xi * xi) + (xi1 * xi1))), 2) - 0.5
        val denom = 1 + 0.001 * pow((xi * xi) - 2 * xi * xi1 + (xi1 * xi1), 2)
        0.5 + numer / denom
    }

  def paviani(x: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)) = {
    val list = x.productIterator.toList.asInstanceOf[List[Double]] // This is safe
    val t1 = mapSum(list)(xi => pow(log(10 - xi), 2) + pow(log(xi - 2), 2))
    val t2 = pow(mapProduct(list)(xi => xi), 0.2)
    t1 - t2
  }

  def penalty1(l: AtLeast2List) = {
    val x = AtLeast2List.unwrap(l)
    def u(xi: Double, a: Int, k: Int, m: Double) =
      if (xi > a) k * pow(xi - a, m)
      else if (xi < -a) k * pow(-xi - a, m)
      else 0.0

    def yi(xi: Double) = 1 + ((xi + 1) / 4)

    val term1 = 10 * pow(sin(Pi * yi(x.head)), 2)
    val term2 = mapSum(pairs(x)) {
      case (xi, xi1) =>
        val t1 = pow(yi(xi) - 1, 2)
        val t2 = 1 + 10 * pow(sin(Pi * yi(xi1)), 2)
        t1 * t2
    }
    val term3 = (yi(x.last) - 1.0)
    val term4 = mapSum(x)(xi => u(xi, 10, 100, 4))

    (Pi / 30) * (term1 + term2 + term3*term3) + term4
  }

  def penalty2(l: AtLeast2List) = {
    val x = AtLeast2List.unwrap(l)
    def u(xi: Double, a: Int, k: Int, m: Double) =
      if (xi > a) k * pow(xi - a, m)
      else if (xi < -a) k * pow(-xi - a, m)
      else 0.0

    val term1 = sin(3.0 * Pi * x.head)
    val term2 = mapSum(pairs(x)) {
      case (xi, xi1) =>
        val t1 = (xi - 1) * (xi - 1)
        val t2 = 1 + pow(sin(3 * Pi * xi1), 2)
        t1 * t2
    }

    val term3 = ((x.last - 1) * (x.last - 1)) * (1 + pow(sin(2 * Pi * x.last), 2))
    val term4 = mapSum(x)(xi => u(xi, 5, 100, 4))
    0.1 * (term1*term1 + term2 + term3) + term4
  }

  def penHolder(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = abs(1 - (sqrt((x1 * x1) + (x2 * x2)) / Pi))
    val t2       = cos(x1) * cos(x2)
    val expon    = pow(abs(exp(t1) * t2), -1)
    -exp(-expon)
  }

  def periodic(x: NonEmptyList[Double]) = {
    val t1 = mapSum(x)(xi => sin(xi) * sin(xi))
    val t2 = 0.1 * exp(-mapSum(x)(square))
    1 + t1 - t2
  }

  def pinter(l: AtLeast2List) = {
    val x = AtLeast2List.unwrap(l).toChunk
    val padded                 = x.last :: (x.toList :+ x.head)
    def A(a0: Double, a1: Double, a2: Double) = a0 * sin(a1) + sin(a2)
    def B(b0: Double, b1: Double, b2: Double) = (b0 * b0) - (2 * b1) + (3 * b2) - cos(b1) + 1
    val t1                     = mapSum(x.zipWithIndex) { case (xi, i) => (i + 1) * (xi * xi) }
    val t2 = mapSum(padded.sliding(3).toList.zipWithIndex) {
      case (Seq(x0, x1, x2), i) => 20 * (i + 1) * pow(sin(A(x0, x1, x2)), 2)
    }
    val t3 = mapSum(padded.sliding(3).toList.zipWithIndex) {
      case (Seq(x0, x1, x2), i) => (i + 1) * log(1 + (i + 1) * pow(B(x0, x1, x2), 2))
    }
    t1 + t2 + t3
  }

  def plateau(x: NonEmptyList[Double]) =
    30 + mapSum(x)(xi => floor(abs(xi)))

  def powell(x: (Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4) = x
    val t1               = (x3 + 10 * x1)
    val t2               = 5 * (x2 - x4)
    val t3               = x1 - 2 * x2
    val t4               = 10 * pow(x3 - x4, 4)
    t1*t1 + t2*t2 + pow(t3, 4) + t4
  }

  def powellSum(x: NonEmptyList[Double]) =
    mapSum(x.zipWithIndex) {
      case (xi, i) => pow(abs(xi), i + 1.0)
    }

  def powerSum(x: (Double, Double, Double, Double)) = {
    val list = x.productIterator.toList.asInstanceOf[List[Double]] // This is safe
    val b = List(8, 18, 44, 114)
    mapSum(b.zipWithIndex) {
      case (bk, i) =>
        val k = i + 1
        val t = mapSum(list)(xi => pow(xi, k.toDouble))
        val r = t - bk
        r * r
    }
  }

  def price1(x: NonEmptyList[Double]) =
    mapSum(x)(xi => square(abs(xi) - 5))

  def price2(x: NonEmptyList[Double]) =
    1 + mapSum(x)(xi => square(sin(xi))) - 0.1 * exp(-mapSum(x)(square))

  def price3(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = 100 * pow(x2 - (x1 * x1), 2)
    val t2       = 6.4 * ((x2 - 0.5) * (x2 - 0.5)) - x1 - 0.6
    t1 + t2*t2
  }

  def price4(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = 2 * (x1 * x1 * x1) * x2 - (x2 * x2 * x2)
    val t2       = 6 * x1 - (x2 * x2) + x2
    t1*t1 + t2*t2
  }

  def qing(x: NonEmptyList[Double]) =
    mapSum(x.zipWithIndex) {
      case (xi, i) => square((xi * xi) - (i + 1))
    }

  def quadratic(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = -3803.84 - 138.08 * x1
    val t2       = -232.92 * x2 + 128.08 * (x1 * x1)
    val t3       = 203.64 * (x2 * x2) + 182.25 * x1 * x2
    t1 + t2 + t3
  }

  def quadric(x: NonEmptyList[Double]) =
    mapSum(1 to x.size) { i =>
      square(mapSum(x.toList take i)(xi => xi))
    }

  def quintic(x: NonEmptyList[Double]) =
    abs(mapSum(x) { xi =>
      pow(xi, 5) - 3 * pow(xi, 4) + 4 * pow(xi, 3) + 2 * pow(xi, 2) - 10 * xi - 4
    })

  def rana(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (xi, xi1) =>
        val t1 = sqrt(abs(xi1 + xi + 1))
        val t2 = sqrt(abs(xi1 - xi + 1))
        (xi1 + 1) * cos(t2) * sin(t1) + xi * cos(t1) * sin(t2)
    }

  def rastrigin(x: NonEmptyList[Double]) =
    10 * x.length + mapSum(x)(xi => xi * xi - 10 * cos(2 * Pi * xi))

  def rosenbrock(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x).toChunk.toList)) {
      case (xi, xi1) =>
        100 * square(xi1 - (xi * xi)) + square(xi - 1)
    }

  def ripple1(x: NonEmptyList[Double]) =
    mapSum(x) { xi =>
      val u = -2 * log(2) * pow((xi - 0.1) / 0.8, 2)
      val v = pow(sin(5 * Pi * xi), 6) + 0.1 * pow(cos(500 * Pi * xi), 2)
      -exp(u) * v
    }

  def ripple2(x: NonEmptyList[Double]) =
    mapSum(x) { xi =>
      val u = -2 * log(2) * pow(((xi - 0.1) / 0.8), 2)
      val v = pow(sin(5 * Pi * xi), 6)
      -exp(u) * v
    }

  def rotatedEllipse1(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (x1, x2) =>
        (7 * (x1 * x1)) - (6 * sqrt(3.0) * x1 * x2) + (13 * (x2 * x2))
    }

  def rotatedEllipse2(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (x1, x2) => (x1 * x1) - (x1 * x2) + (x2 * x2)
    }

  def salomon(x: NonEmptyList[Double]) = {
    val ss = sqrt(spherical(x))
    -cos(2 * Pi * ss) + (0.1 * ss) + 1
  }

  def sargan(x: NonEmptyList[Double]) = {
    val zipped = x.zipWithIndex
    mapSum(zipped) {
      case (xi, i) =>
        val sum = mapSum(zipped.filter { case (_, j) => i != j }) { case (xj, _) => xi * xj }
        x.size * ((xi * xi) + 0.4 * sum)
    }
  }

  def schaffer1(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (xi, xi1) =>
        val t0 = (xi * xi) + (xi1 * xi1)

        val t3 = pow(sin(t0*t0), 2) - 0.5
        val t4 = pow(1 + 0.001 * t0, 2)
        0.5 + (t3 / t4)
    }

  def schaffer2(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (xi, xi1) =>
        val t0 = (xi * xi) - (xi1 * xi1)

        val t3 = pow(sin(t0 * t0), 2) - 0.5
        val t4 = pow(1 + 0.001 * t0, 2)
        0.5 + (t3 / t4)
    }

  def schaffer3(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (xi, xi1) =>
        val t1 = cos(abs((xi * xi) - (xi1 * xi1)))
        val t2 = (xi * xi) + (xi1 * xi1)
        val t3 = pow(sin(t1 * t1), 2) - 0.5
        val t4 = pow(1 + 0.001 * t2, 2)
        0.5 + (t3 / t4)
    }

  def schaffer4(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (xi, xi1) =>
        val t0 = sin((xi * xi) - (xi1 * xi1))

        val t3 = pow(cos(t0 * t0), 2) - 0.5
        val t4 = pow(1 + 0.001 * t0, 2)
        0.5 + (t3 / t4)
    }

  def schaffer6(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (xi, xi1) =>
        val t0 = (xi * xi) + (xi1 * xi1)

        val t3 = pow(sin(sqrt(t0)), 2) - 0.5
        val t4 = pow(1 + 0.001 * t0, 2)
        0.5 + (t3 / t4)
    }

  def schumerSteiglitz(x: NonEmptyList[Double]) =
    mapSum(x)(xi => pow(xi, 4))

  def schwefel1(x: NonEmptyList[Double]) =
    pow(mapSum(x)(square), sqrt(Pi))

  def schwefel12(x: NonEmptyList[Double]) =
    mapSum(x.zipWithIndex) {
      case (xi, i) => pow(mapSum(x.take(i + 1))(xi => xi), 2)
    }

  def schwefel220(x: NonEmptyList[Double]) =
    mapSum(x)(abs(_))

  def schwefel221(x: NonEmptyList[Double]) =
    x.fold(abs(x.head)) { (xi, xi1) =>
      math.max(abs(xi), abs(xi1))
    }

  def schwefel222(x: NonEmptyList[Double]) =
    mapSum(x)(abs(_)) + mapProduct(x)(abs(_))

  def schwefel223(x: NonEmptyList[Double]) =
    mapSum(x)(xi => pow(xi, 10))

  def schwefel225(x: NonEmptyList[Double]) =
    mapSum(x)(xi => ((xi - 1) *  (xi - 1)) + pow((x.head - (xi * xi)), 2))

  def schwefel226(x: NonEmptyList[Double]) =
    418.9829 * x.size - mapSum(x)(xi => xi * sin(sqrt(abs(xi))))

  def schwefel236(x: (Double, Double)) = {
    val (x1, x2) = x
    -x1 * x2 * (72 - 2 * x1 - 2 * x2)
  }

  def schwefel24(x: NonEmptyList[Double]) =
    mapSum(x)(xi => ((x.head - 1) * (x.head - 1)) + ((x.head - xi) * (x.head - xi)))

  def schwefel26(x: (Double, Double)) = {
    val (x1, x2) = x
    math.max(abs(x1 + 2 * x2 - 7), abs(2 * x1 + x2 - 5))
  }

  def shekel5(x: (Double, Double, Double, Double)) = {
    val list = List(x._1, x._2, x._3, x._4)
    val a = List(
      List(4, 4, 4, 4),
      List(1, 1, 1, 1),
      List(8, 8, 8, 8),
      List(6, 6, 6, 6),
      List(3, 7, 3, 7)
    )
    val c = List(0.1, 0.2, 0.2, 0.4, 0.6)

    -mapSum(a zip c) {
      case (ai, ci) =>
        1 / (ci + mapSum(list zip ai) { case (xj, aij) => (xj - aij) * (xj - aij) })
    }
  }

  def shekel7(x: (Double, Double, Double, Double)) = {
    val list = List(x._1, x._2, x._3, x._4)
    val a = List(
      List(4, 4, 4, 4),
      List(1, 1, 1, 1),
      List(8, 8, 8, 8),
      List(6, 6, 6, 6),
      List(3, 7, 3, 7),
      List(2, 9, 2, 9),
      List(5, 5, 3, 3)
    )
    val c = List(0.1, 0.2, 0.2, 0.4, 0.4, 0.6, 0.3)

    -mapSum(a zip c) {
      case (ai, ci) =>
        1.0 / (ci + mapSum(list zip ai) { case (xj, aij) => (xj - aij) * (xj - aij) })
    }
  }

  def shekel10(x: (Double, Double, Double, Double)) = {
    val list = List(x._1, x._2, x._3, x._4)
    val a = List(
      List(4.0, 4.0, 4.0, 4.0),
      List(1.0, 1.0, 1.0, 1.0),
      List(8.0, 8.0, 8.0, 8.0),
      List(6.0, 6.0, 6.0, 6.0),
      List(3.0, 7.0, 3.0, 7.0),
      List(2.0, 9.0, 2.0, 9.0),
      List(5.0, 5.0, 3.0, 3.0),
      List(8.0, 1.0, 8.0, 1.0),
      List(6.0, 2.0, 6.0, 2.0),
      List(7.0, 3.6, 7.0, 3.6)
    )
    val c = List(0.1, 0.2, 0.2, 0.4, 0.4, 0.6, 0.3, 0.7, 0.5, 0.5)

    -mapSum(a zip c) {
      case (ai, ci) =>
        1.0 / (ci + mapSum(list zip ai) { case (xj, aij) => (xj - aij) * (xj - aij) })
    }
  }

  def shubert(x: NonEmptyList[Double]) =
    -mapProduct(x) { xi =>
      mapSum(1 to 5)(j => j * cos((j + 1) * xi + j))
    }

  def shubert1(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = mapSum(1 to 5)(j => j * cos((j + 1) * x1 + j))
    val t2       = mapSum(1 to 5)(j => j * cos((j + 1) * x2 + j))
    t1 * t2
  }

  def shubert3(x: (Double, Double)) =
    mapSum(List(x._1, x._2))(xi => mapSum(1 to 5)(j => -j * sin((j + 1) * xi + j)))

  def shubert4(x: (Double, Double)) =
    mapSum(List(x._1, x._2))(xi => mapSum(1 to 5)(j => -j * cos((j + 1) * xi + j)))

  def sineEnvelope(x: (Double, Double)) = {
    val (x1, x2) = x
    val numer    = pow(sin(sqrt((x1 * x1) + (x2 * x2))), 2) - 0.5
    val denom    = 1 + 0.0001 * pow((x1 * x1) + (x2 * x2), 2)
    0.5 + (numer / denom)
  }

  def sixHumpCamelback(x: (Double, Double)) = {
    val (x1, x2) = x
    val tX1      = 4 * (x1 * x1) - 2.1 * pow(x1, 4) + (pow(x1, 6) / 3.0)
    val tX2      = x1 * x2 - 4 * (x2 * x2) + 4 * pow(x2, 4)
    tX1 + tX2
  }

  def spherical(x: NonEmptyList[Double]) =
    mapSum(x)(square)

  def step1(x: NonEmptyList[Double]) =
    mapSum(x)(xi => floor(abs(xi)))

  def step2(x: NonEmptyList[Double]) =
    mapSum(x)(xi => pow(floor(xi) + 0.5, 2))

  def step3(x: NonEmptyList[Double]) =
    mapSum(x)(xi => floor(xi * xi))

  def stretchedVSineWave(x: AtLeast2List) =
    mapSum(pairs(AtLeast2List.unwrap(x))) {
      case (xi, xi1) =>
        val t1 = pow((xi1 * xi1) + (xi * xi), 0.25)
        val t2 = pow(sin(50 * (((xi1 * xi1) + pow(xi * xi, 0.1)))), 2) + 0.1
        t1 * t2
    }

  def styblinksiTang(x: NonEmptyList[Double]) =
    0.5 * mapSum(x)(xi => pow(xi, 4) - 16 * pow(xi, 2) + 5 * xi)

  def sumSquares(x: NonEmptyList[Double]) =
    mapSum(x.zipWithIndex) { case (xi, i) => (i + 1) * (xi * xi) }

  def sumDifferentPowers(x: NonEmptyList[Double]) =
    mapSum(x.zipWithIndex) { case (xi, i) => pow(abs(xi), (i + 2.0)) }

  def threeHumpCamelback(x: (Double, Double)) = {
    val (x1, x2) = x
    2 * (x1 * x1) - 1.05 * pow(x1, 4) + (pow(x1, 6) / 6) + x1 * x2 + x2 * x2
  }

  def trecanni(x: (Double, Double)) = {
    val (x1, x2) = x
    pow(x1, 4) + 4 * pow(x1, 3) + 4 * (x1 * x1) + (x2 * x2)
  }

  def trefethen(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = 0.25 * (x1 * x1) + 0.25 * (x2 * x2)
    val t2       = exp(sin(50 * x1)) - sin(10 * (x1 + x2))
    val t3       = sin(60 * exp(x2))
    val t4       = sin(70 * sin(x1))
    val t5       = sin(sin(80 * x2))
    t1 + t2 + t3 + t4 + t5
  }

  def trid(x: AtLeast2List) = {
    val t1 = mapSum(AtLeast2List.unwrap(x))(xi => (xi - 1) * (xi - 1))
    val t2 = mapSum(pairs(AtLeast2List.unwrap(x))) { case (xi, xi1) => xi * xi1 }
    t1 - t2
  }

  def trigonometric1(x: NonEmptyList[Double]) =
    mapSum(x.zipWithIndex) {
      case (xi, i) =>
        pow(x.size - mapSum(x)(cos(_)) + i * (1.0 - cos(xi) - sin(xi)), 2)
    }

  def trigonometric2(x: NonEmptyList[Double]) =
    1.0 + mapSum(x) { xi =>
      val co = pow(xi - 0.9, 2)
      val t1 = 8 * pow(sin(7 * co), 2)
      val t2 = 6 * pow(sin(14 * co), 2)
      t1 + t2 + co
    }

  def tripod(x: (Double, Double)) = {
    val (x1, x2) = x
    def p(xi: Double) = if (xi >= 0.0) 1.0 else 0.0

    val t1 = p(x2) * (1 + p(x2))
    val t2 = abs(x1 + 50 * p(x2) * (1 - 2 * p(x1)))
    val t3 = abs(x2 + 50 * (1 - 2 * p(x2)))
    t1 + t2 + t3
  }

  def unevenDecreasingMaxima(x: Double) = {
    val t1 = exp(-2 * log(2) * pow((x - 0.08) / 0.854, 2))
    val t2 = pow(sin(5 * Pi * pow(x, 3.0 / 4.0) - 0.05), 6)
    t1 * t2
  }

  def ursem1(x: (Double, Double)) = {
    val (x1, x2) = x
    -sin(2 * x1 - 0.5 * Pi) - 3 * cos(x2) - 0.5 * x1
  }

  def ursem3(x: (Double, Double)) = {
    val (x1, x2) = x
    val co1      = -sin(2.2 * Pi * x1 + 0.5 * Pi)
    val co2      = -sin(2.2 * Pi * x2 + 0.5 * Pi)
    val t1       = co1 * ((2 - abs(x1)) / (2)) * ((3 - abs(x1)) / (2))
    val t2       = co2 * ((2 - abs(x2)) / (2)) * ((3 - abs(x2)) / (2))
    t1 + t2
  }

  def ursem4(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = -3 * sin(0.5 * Pi * x1 + 0.5 * Pi)
    val t2       = (2 - sqrt((x1 * x1) + (x2 * x2))) / 4
    t1 * t2
  }

  def ursemWaves(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = -0.9 * (x1 * x1)
    val t2       = ((x2 * x2) - 4.5 * (x2 * x2)) * x1 * x2
    val t3       = 4.7 * cos(3 * x1 - (x2 * x2) * (2 + x1))
    val t4       = sin(2.5 * Pi * x1)
    t1 + t2 + t3 * t4
  }

  def venterSobiezcczanskiSobieski(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = (x1 * x1) - 100 * (cos(x1) * cos(x1))
    val t2       = -100 * cos((x1 * x1) / 30) + (x2 * x2)
    val t3       = -100 * (cos(x2) * cos(x2)) - 100 * cos((x2 * x2) / 30)
    t1 + t2 + t3
  }

  def vincent(x: NonEmptyList[Double]) =
    -mapSum(x)(xi => sin(10 * log(xi)))

  def watson(x: (Double, Double, Double, Double, Double, Double)) = {
    val list = NonEmptyList(x._1, x._2, x._3, x._4, x._5, x._6)
    val x1 = x._1

    val t1 = mapSum(0 to 29) { i =>
      val ai = i / 29.0
      val sum4 = mapSum(list.tail.zipWithIndex) {
        case (xj, j) =>
          (j + 1) * pow(ai, j.toDouble) * xj
      }
      val sum5 = mapSum(list.zipWithIndex) {
        case (xk, k) =>
          pow(ai, k.toDouble) * xk
      }
      pow(sum4 - (sum5 * sum5) - 1, 2)
    }
    t1 + (x1 * x1)
  }

  def wayburnSeader1(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = pow(pow(x1, 6) + pow(x2, 4) - 17, 2)
    val t2       = pow(2 * x1 + x2 - 4, 2)
    t1 + t2
  }

  def wavy(k: Double = 10.0)(x: NonEmptyList[Double]) =
    1 - (mapSum(x) { xi =>
      cos(k * xi) * exp(-(xi * xi) / 2)
    } / x.size)

  def wayburnSeader2(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = pow(1.613 - 4 * pow(x1 - 0.3125, 2) - 4 * pow(x2 - 1.625, 2), 2)
    val t2       = (x2 - 1) * (x2 - 1)
    t1 + t2
  }

  def wayburnSeader3(x: (Double, Double)) = {
    val (x1, x2) = x
    val t1       = 2 * (pow(x1, 3) / 3) - 8 * (x1 * x1) + 33 * x1 - x1 * x2 + 5
    val t2       = pow(((x1 - 4) * (x1 - 4)) + ((x2 - 5.0) * (x2 - 5.0) - 4), 2)
    t1 + t2
  }

  def weierstrass(x: NonEmptyList[Double]) = {
    val a    = 0.5
    val b    = 3.0
    val kmax = 20
    val constant = (0 to kmax).toList.map { k =>
      val t1 = pow(a, k.toDouble)
      val t2 = cos(Pi * pow(b, k.toDouble))
      t1 * t2
    }.sum

    val factor1 = mapSum(x) { xi =>
      mapSum(0 to kmax) { k =>
        val t1 = pow(a, k.toDouble)
        val t2 = cos(2 * Pi * pow(b, k.toDouble) * (xi + 0.5))
        t1 * t2
      }
    }

    factor1 - x.size * constant
  }

  def whitley(x: NonEmptyList[Double]) =
    mapSum(x) { xi =>
      mapSum(x) { xj =>
        val factor = 100 * pow((xi * xi) - xj, 2) + pow(1 - xj, 2)
        val t1     = (factor * factor) / 4000
        val t2     = cos(factor)
        t1 - t2 + 1
      }
    }

  def wolfe(x: (Double, Double, Double)) = {
    val (x1, x2, x3) = x
    (4.0 / 3.0) * pow((x1 * x1) + (x2 * x2), 0.75) + x3
  }

  def wood(x: (Double, Double, Double, Double)) = {
    val (x1, x2, x3, x4) = x
    val t1               = 100 * pow(((x1 * x1) - x2), 2)
    val t2               = pow(x1 - 1, 2) + pow(x3 - 1, 2)
    val t3               = 90 * pow(x3 * x3 - x4, 2)
    val t4               = 10.1 * pow(x2 - 1, 2)
    val t5               = pow(x4 - 1, 2)  + 19.8 * (x2 - 1) * (x4 - 1)
    t1 + t2 + t3 + t4 + t5
  }

  def xinSheYang2(x: NonEmptyList[Double]) = {
    val t1 = mapSum(x)(abs(_))
    val t2 = exp(-mapSum(x)(xi => sin(square(xi))))
    t1 * t2
  }

  def xinSheYang3(m: Double = 5.0)(x: NonEmptyList[Double]) = {
    val β = 15.0
    val u = mapSum(x)(xi => pow(xi / β, 2 * m))
    val v = mapSum(x)(square)
    val w = mapProduct(x)(xi => cos(xi) * cos(xi))
    exp(-u) - 2 * exp(-v) * w
  }

  def xinSheYang4(x: NonEmptyList[Double]) = {
    val u = mapSum(x)(xi => sin(xi) * sin(xi))
    val v = mapSum(x)(square)
    val w = mapSum(x)(xi => pow(sin(sqrt(abs(xi))), 2))
    (u - exp(-v)) * exp(-w)
  }

  def yaoLiu4(x: NonEmptyList[Double]) =
    x.map(abs(_)).max

  def yaoLiu9(x: NonEmptyList[Double]) =
    mapSum(x)(xi => (xi * xi) - 10 * cos(2 * Pi * xi) + 10)

  def zakharov(x: NonEmptyList[Double]) = {
    val t = mapSum(x.zipWithIndex) { case (xi, i) => 0.5 * i * xi }
    spherical(x) + pow(t, 2) + pow(t, 4)
  }

  def zeroSum(x: NonEmptyList[Double]) = {
    val sum = mapSum(x)(xi => xi)
    if (sum == 0.0) 0.0
    else 1 + sqrt(10000 * abs(sum))
  }

  def zettle(x: (Double, Double)) = {
    val (x1, x2) = x
    pow(x1 * x1 + x2 * x2 - 2 * x1, 2) + x1 / 4
  }

  def zirilli1(x: (Double, Double)) = {
    val (x1, x2) = x
    0.25 * pow(x1, 4) - 0.5 * pow(x1, 2) + 0.1 * x1 + 0.5 * (x2 * x2)
  }

  def zirilli2(x: (Double, Double)) = {
    val (x1, x2) = x
    0.5 * (x1 * x1) + 0.5 * (1.0 - cos(2 * x1)) + (x2 * x2)
  }

}
