package benchmarks

import shapeless._

import org.scalacheck._
import org.scalacheck.Prop._

import spire.implicits._
import spire.math._

import Benchmarks._
import implicits._
import dimension.Generators._

object BenchmarksTest extends Properties("Benchmarks") {
  val zero3 = Sized(0.0, 0.0, 0.0)

  val epsilon = 1e-15
  def epsilonF(precision: Double) = 1.0 / (10.0 ** precision)

  implicit class DoubleEpsilonOps(val d: Double) extends AnyVal {
    def ~(v: Double, e: Double) = abs(v - d) <= e
    def ~(v: Double) = abs(v - d) <= epsilon
  }

  property("absoluteValue") = forAll(genSized(-100.0, 100.0)) { g =>
    val abs = absoluteValue(g)
    abs === absoluteValue(g.map(_ * -1)) &&
    abs >= 0.0 &&
    abs >= g.mapSum(xi => xi)
  } && {
    absoluteValue(zero3) === 0.0 &&
    absoluteValue(Sized(1.0, 2.0, 3.0)) === 6.0 &&
    absoluteValue(Sized(-1.0, -2.0, -3.0)) === 6.0
  }

  property("ackley") = forAll(genSized(-32.768, 32.768)) { g =>
    ackley(g) >= 0.0
  } && ackley(zero3) ~ epsilon

  property("ackley3") = forAll(gen2(-32.768, 32.768)) { g =>
    ackley2(g) >= -200.0 &&
    ackley3(g) >= -195.62902825923879
  } && {
    ackley2(Sized(0.0, 0.0)) === -200.0 &&
    ackley3(Sized(-0.68255758, -0.36070859)) ~ (-195.62902825923879, epsilonF(10))
  }

  property("adjiman") = forAll(gen2(-5.0, 5.0)) { g =>
    adjiman(g) >= -5.02181
  } && adjiman(Sized(2.0, 0.10578)) === -2.0218067833370204

  property("alpine1") = forAll(genSized(-10.0, 10.0)) { g =>
    alpine1(g) >= 0.0
  } && alpine1(zero3) === 0.0

  property("alpine2") = forAll(genSized(0.0, 10.0)) { g =>
    alpine2(g) <= g.mapProduct(sqrt)
  } && alpine2(Sized(7.91705268, 4.81584232)) ~ (-6.1295, epsilonF(3))

  property("arithmeticMean") = forAll(genSized(0.0, 1.0)) { g =>
    arithmeticMean(g) >= 0.0
  } && arithmeticMean(zero3) === 0.0

  property("bartelsConn") = forAll(gen2(-50.0, 50.0)) { g =>
    bartelsConn(g) >= 1.0
  } && bartelsConn(Sized(0.0, 0.0)) === 1.0

  property("beale") = forAll(gen2(-4.5, 4.5)) { g =>
    beale(g) >= 0.0
  } && beale(Sized(3.0, 0.5)) === 0.0

  property("biggsEXP2") = forAll(gen2(0.0, 20.0)) { g =>
    biggsEXP2(g) >= 0.0
  } && biggsEXP2(Sized(1.0, 10.0)) === 0.0

  property("biggsEXP3") = forAll(gen3(0.0, 20.0)) { g =>
    biggsEXP3(g) >= 0.0
  } && biggsEXP3(Sized(1.0, 10.0, 5.0)) === 0.0

  property("biggsEXP4") = forAll(gen4(0.0, 20.0)) { g =>
    biggsEXP4(g) >= 0.0
  } && biggsEXP4(Sized(1.0, 10.0, 1.0, 5.0)) === 0.0

  property("biggsEXP5") = forAll(gen5(0.0, 20.0)) { g =>
    biggsEXP5(g) >= 0.0
  } && biggsEXP5(Sized(1.0, 10.0, 1.0, 5.0, 4.0)) === 0.0

  property("biggsEXP6") = forAll(gen6(-20.0, 20.0)) { g =>
    biggsEXP6(g) >= 0.0
  } && biggsEXP6(Sized(1.0, 10.0, 1.0, 5.0, 4.0, 3.0)) === 0.0

  property("bird") = forAll(gen2(-2.0 * pi, 2.0 * pi)) { g =>
    bird(g) >= -106.764537
  } && {
    bird(Sized(4.70104, 3.15294)) ~ (-106.764537, epsilonF(5)) &&
    bird(Sized(-1.58214, -3.13024)) ~ (-106.764537, epsilonF(5))
  }

  property("bohachevsky") = forAll(gen2(-100.0, 100.0)) { g =>
    bohachevsky1(g) >= 0.0 &&
    bohachevsky2(g) >= 0.0 &&
    bohachevsky3(g) >= 0.0
  } && {
    val zero2 = Sized(0.0, 0.0)
    bohachevsky1(zero2) === 0.0 &&
    bohachevsky2(zero2) === 0.0 &&
    bohachevsky3(zero2) === 0.0
  }

  property("booth") = forAll(gen2(-10.0, 10.0)) { g =>
    booth(g) >= 0.0
  } && {
    booth(Sized(1.0, 3.0)) === 0.0
  }

  property("boxBettsQuadraticSum") = forAll(gen3D((0.9, 1.2), (9.0, 11.2), (0.9, 1.2))) { g =>
    boxBettsQuadraticSum(10)(g) >= 0.0
  } && {
    boxBettsQuadraticSum(10)(Sized(1.0, 10.0, 1.0)) === 0.0
  }

  property("brad") = forAll(gen3D((-0.25, 0.25), (0.01, 2.5), (0.01, 2.5))) { g =>
    brad(g) >= 0.00821487
  }

  property("braninRCOS1") = forAll(gen2D((-5.0, 10.0), (0.0, 15.0))) { g =>
    braninRCOS1(g) >= 0.3978874 - epsilon
  } && braninRCOS1(Sized(-pi, 12.275)) ~ (0.3978874, epsilonF(5))

  property("braninRCOS2") = forAll(gen2(-5.0, 15.0)) { g =>
    braninRCOS2(g) >= 5.559037
  } && {
    braninRCOS2(Sized(-3.2, 12.53)) ~ (5.559037, epsilonF(6))
  }

  property("brent") = forAll(genSized(-10.0, 10.0)) { g =>
    brent(g) >= 0.0
  } && brent(Sized(-10.0, -10.0, -10.0)) ~ (0.0, epsilon)

  property("brown") = forAll(genSized(-1.0, 1.0)) { g =>
    brown(g) >= 0.0
  } && brown(Sized(0.0, 0.0, 0.0)) === 0.0

  property("bukin") = forAll(gen2D((-15.0, -5.0), (-3.0, 3.0))) { g =>
    bukin2(g) >= 0.0 &&
    bukin2Adapted(g) >= 0.0 &&
    bukin4(g) >= 0.0 &&
    bukin6(g) >= 0.0
  } && {
    val g = Sized(-10.0, 0.0)
    bukin2(g)            === 0.0 &&
    bukin2Adapted(g)     === 0.0 &&
    bukin4(g)            === 0.0 &&
    bukin6(Sized(-10.0, 1.0)) === 0.0
  }

  property("carromTable") = forAll(gen2(-10.0, 10.0)) { g =>
    carromTable(g) >= -24.15681551650653
  } && {
    carromTable(Sized(9.646157266348881, 9.646134286497169))   === -24.15681551650653 &&
    carromTable(Sized(-9.646157266348881, 9.646134286497169))  === -24.15681551650653 &&
    carromTable(Sized(9.646157266348881, -9.646134286497169))  === -24.15681551650653 &&
    carromTable(Sized(-9.646157266348881, -9.646134286497169)) === -24.15681551650653
  }

  property("centralTwoPeakTrap") = forAll(Gen.choose(0.0, 20.0)) { g =>
    centralTwoPeakTrap(g) >= -200.0
  } && {
    centralTwoPeakTrap((-1.0)) === 0.0 &&
    centralTwoPeakTrap((20.0)) === -200.0 &&
    centralTwoPeakTrap((21.0)) === -200.0
  }

  property("chichinadze") = forAll(gen2(-30.0, 30.0)) { g =>
    chichinadze(g) >= -43.3159
  } && chichinadze(Sized(5.90133, 0.5)) ~ (-43.3159, epsilonF(4))

  property("chungReynolds") = forAll(genSized(-100.0, 100.0)) { g =>
    chungReynolds(g) >= 0.0
  } && chungReynolds(zero3) === 0.0

  property("cigar") = forAll(genSized(-100.0, 100.0)) { g =>
    cigar(10e6)(g) >= 0.0
  } && cigar(10e6)(Sized(0.0, 0.0, 0.0)) === 0.0

  property("colville") = forAll(gen4(-10.0, 10.0)) { g =>
    colville(g) >= 0.0
  } && {
    colville(Sized(1.0, 1.0, 1.0, 1.0)) === 0.0 &&
    colville(Sized(0.0, 0.0, 0.0, 0.0)) === 42.0
  }

  property("corana") = forAll(gen4(-5.0, 5.0)) { g =>
    corana(0.05)(g) >= 0.0
  } && corana(0.05)(Sized(0.0, 0.0, 0.0, 0.0)) === 0.0

  property("cosineMixture") = forAll(genSized(-1.0, 1.0)) { g =>
    cosineMixture(g) >= -0.1 * g.size
  } && cosineMixture(zero3) ~ (-0.1 * zero3.size, epsilonF(5))

  property("cross") = forAll(genSized(-10.0, 10.0)) { g =>
    crossInTray(g) >= -2.11 &&
    crossLegTable(g) >= -1.0 &&
    crossCrowned(g) >= -0.0001
  } && {
    crossInTray(Sized(1.349406685353340,1.349406608602084)) ~ (-2.06261218, epsilonF(6)) &&
    crossLegTable(zero3) === -1.0 &&
    crossCrowned(zero3) === 0.0001
  }

  property("csendes") = forAll(genSized(-1.0, 1.0)) { g =>
    val fit = csendes(g)
    if (g.exists(_ == 0.0))
      fit == None
    else
      fit.forall(_ >= 0.0)
  } && csendes(zero3) == None

  property("cube") = forAll(gen2(-10.0, 10.0)) { g =>
    cube(g) >= 0.0
  } && {
    cube(Sized(1.0, 1.0)) === 0.0 &&
    cube(Sized(-1.0, 1.0)) === 404.0
  }

  property("damavandi") = forAll(gen2(0.0, 14.0)) { g =>
    damavandi(g).forall(_ >= 0.0)
  } && {
    damavandi(Sized(2.0, 2.0)) == None
  }

  property("deb") = forAll(genSized(0.0, 1.0)) { g =>
    deb1(g) >= -1.0 &&
    deb2(g) >= -1.0
  } && deb1(zero3) === 0.0

  property("decanomial") = forAll(gen2(-10.0, 10.0)) { g =>
    decanomial(g) >= 0.0
  } && decanomial(Sized(2.0, -3.0)) === 0.0

  property("deckkersAarts") = forAll(gen2(-20.0, 20.0)) { g =>
    deckkersAarts(g) >= -24777.0
  } && {
    deckkersAarts(Sized(0.0, 15.0)) ~ (-24771.0, epsilonF(0)) &&
    deckkersAarts(Sized(0.0, -15.0)) ~ (-24771.0, epsilonF(0))
  }

  property("deflectedCorrugatedSpring") = forAll(genSized(0.0, 10.0)) { g =>
    deflectedCorrugatedSpring(5.0)(g) >= -0.1 * g.size
  } && forAll(genConst(5.0)) { g =>
    deflectedCorrugatedSpring(5.0)(g) === -0.1 * g.size
  }

  property("deVilliersGlasser1") = forAll(gen4(1.0, 100.0)) { g =>
    deVilliersGlasser1(g) >= 0.0
  }

  property("deVilliersGlasser2") = forAll(gen5(1.0, 6.0)) { g =>
    deVilliersGlasser2(g) >= 0.0
  }

  property("differentPowers") = forAll(genSized(-100.0, 100.0)) { g =>
    differentPowers(g) >= 0.0
  } && differentPowers(Sized(0.0, 0.0, 0.0)) === 0.0

  property("discus") = forAll(genSized(-100.0, 100.0)) { g =>
    discus(g) >= 0.0
  } && {
    discus(Sized(0.0))      === 0.0 &&
    discus(Sized(1.0))      === 1e6 &&
    discus(Sized(1.0, 1.0)) === 1e6 + 1.0
  }

  property("dixonPrice") = forAll(genSized(-10.0, 10.0)) { g =>
    dixonPrice(g) >= 0.0
  } && dixonPrice(Sized(1.0, 1.0 / sqrt(2))) ~ (0.0, epsilon)

  property("dolan") = forAll(gen5(-100.0, 100.0)) { g =>
    dolan(g) >= 0.0
  } && dolan(Sized(8.39045925, 4.81424707, 7.34574133, 68.88246895, 3.85470806)) ~ (0.0, epsilonF(6))

  property("dropWave") = forAll(genSized(-5.12, 5.12)) { g =>
    dropWave(g) >= -1.0
  } && dropWave(zero3) === -1.0

  property("easom") = forAll(gen2(-100.0, 100.0)) { g =>
    easom(g) >= -1.0
  } && easom(Sized(pi, pi)) === -1.0

  property("eggCrate") = forAll(genSized(-5.0, 5.0)) { g =>
    eggCrate(g) >= 0.0
  } && eggCrate(zero3) === 0.0

  property("eggHolder") = forAll(genSized(-512.0, 512.0)) { g =>
    eggHolder(g) >= -959.64 * g.size
  } && eggHolder(Sized(512.0, 404.2319)) ~ (-959.64, epsilonF(3))

  property("elAttarVidyasagarDutta") = forAll(gen2(-100.0, 100.0)) { g =>
    elAttarVidyasagarDutta(g) >= 1.712780354
  } && elAttarVidyasagarDutta(Sized(3.40918683, -2.17143304)) ~ (1.712780354, epsilonF(9))

  property("elliptic") = forAll(genSized(-100.0, 100.0)) { g =>
    elliptic(g) >= 0.0
  } && elliptic(Sized(0.0, 0.0, 0.0)) === 0.0

  property("exponential1") = forAll(genSized(-1.0, 1.0)) { g =>
    exponential1(g) >= -1.0
  } && exponential1(zero3) === -1.0

  property("exponential2") = forAll(gen2(0.0, 20.0)) { g =>
    exponential2(g) >= 0.0
  } && exponential2(Sized(1.0, 10.0)) ~ epsilon

  property("freudensteinRoth") = forAll(gen2(-10.0, 10.0)) { g =>
    freudensteinRoth(g) >= 0.0
  } && freudensteinRoth(Sized(5.0, 4.0)) === 0.0

  property("gear") = forAll(gen4(12.0, 60.0)) { g =>
    gear(g) >= 2.7 * 10e-12
  } && gear(Sized(16.0, 19.0, 43.0, 49.0)) ~ (2.7 * 10e-12, epsilonF(10))

  property("giunta") = forAll(gen2(-1.0, 1.0)) { g =>
    giunta(g) >= 0.06447042053690566
  } && giunta(Sized(0.45834282, 0.45834282)) ~ (0.06447042053690566, epsilonF(3))

  property("goldsteinPrice1") = forAll(gen2(-2.0, 2.0)) { g =>
    goldsteinPrice1(g) >= 3.0
  } && {
    goldsteinPrice1(Sized(1.2, 0.8)) ~ (840.0, epsilonF(12)) &&
    goldsteinPrice1(Sized(1.8, 0.2)) ~ (84.0, epsilonF(12)) &&
    goldsteinPrice1(Sized(-0.6, -0.4)) === 30.0 &&
    goldsteinPrice1(Sized(0.0, -1.0))  === 3.0
  }

  property("goldsteinPrice2") = forAll(gen2(-5.0, 5.0)) { g =>
    goldsteinPrice2(g) >= 1.0
  } && {
    goldsteinPrice2(Sized(3.0, 4.0)) === 1.0
  }

  property("griewank") = forAll(genSized(-600.0, 600.0)) { g =>
    griewank(g) >= 0.0
  } && griewank(zero3) === 0.0

  property("gulf") = forAll(gen3D((0.1, 100.0), (0.0, 25.6), (0.0, 5.0))) { g =>
    gulf(g) >= 0.0
  } && gulf(Sized(50.0, 25.0, 1.5)) ~ 0.0

  property("hansen") = forAll(gen2(-10.0, 10.0)) { g =>
    hansen(g) >= -176.54
  } && {
    hansen(Sized(-7.58993, -7.708314)) ~ (-176.54, epsilonF(2)) &&
    hansen(Sized(-7.58993, -1.425128)) ~ (-176.54, epsilonF(2)) &&
    hansen(Sized(-7.58993, 4.858057))  ~ (-176.54, epsilonF(2)) &&
    hansen(Sized(-1.306708, -7.708314))~ (-176.54, epsilonF(2)) &&
    hansen(Sized(-1.306708, 4.858057)) ~ (-176.54, epsilonF(2)) &&
    hansen(Sized(4.976478, 4.858057))  ~ (-176.54, epsilonF(2)) &&
    hansen(Sized(4.976478, -1.425128)) ~ (-176.54, epsilonF(2)) &&
    hansen(Sized(4.976478, -7.708314)) ~ (-176.54, epsilonF(2))
  }

  property("hartman3") = forAll(gen3(0.0, 1.0)) { g =>
    hartman3(g) >= -3.862782
  } && hartman3(Sized(0.1140, 0.556, 0.852)) ~ (-3.862782, epsilonF(4))

  property("hartman6") = forAll(gen6(-5.0, 5.0)) { g =>
    hartman6(g) >= -3.32236
  } && hartman6(Sized(0.201690, 0.150011, 0.476874,
    0.275332, 0.311652, 0.657301)) ~ (-3.32236, epsilonF(5))

  property("helicalValley") = forAll(gen3(-10.0, 10.0)) { g =>
    helicalValley(g) >= 0.0
  } && {
    helicalValley(Sized(1.0, 0.0, 0.0)) === 0.0
  }

  property("himmelblau") = forAll(gen2(-6.0, 6.0)) { g =>
    himmelblau(g) >= 0.0
  } && himmelblau(Sized(3.0, 2.0)) === 0.0

  property("hosaki") = forAll(gen2(0.0, 10.0)) { g =>
    hosaki(g) >= -2.3458
  } && hosaki(Sized(4.0, 2.0)) ~ (-2.3458, epsilonF(4))

  property("hyperEllipsoid") = forAll(genSized(-10.0, 10.0)) { g =>
    hyperEllipsoid(g.map(-_)) == hyperEllipsoid(g) &&
    hyperEllipsoid(g) >= 0.0
  } && hyperEllipsoid(zero3) === 0.0

  property("hyperEllipsoidRotated") = forAll(genSized(-65.536, 65.536)) { g =>
    hyperEllipsoidRotated (g) >= 0.0
  } && hyperEllipsoidRotated(zero3) === 0.0

  property("jennrichSampson") = forAll(gen2(-1.0, 1.0)) { g =>
    jennrichSampson(g) >= 124.3612
  } && jennrichSampson(Sized(0.257825, 0.257825)) ~ (124.3612, epsilonF(3))

  property("judge") = forAll(gen2(-10.0, 10.0)) { g =>
    judge(g) >= 16.0817307
  } && judge(Sized(0.86479, 1.2357)) ~ (16.0817307, epsilonF(4))

  property("katsuura") = forAll(genSized(0.0, 100.0)) { g =>
    katsuura(g) >= 1.0
  } && katsuura(zero3) === 1.0

  property("keane") = forAll(gen2(0.0, 10.0)) { g =>
    keane(g) >= -0.673668
  } && {
    keane(Sized(0.0, 1.39325)) ~ (0.673668, epsilonF(5)) &&
    keane(Sized(1.39325, 0.0)) ~ (0.673668, epsilonF(5))
  }

  property("kowalik") = forAll(gen4(-5.0, 5.0)) { g =>
    kowalik(g) >= 0.0003074861
  } && kowalik(Sized(0.192833, 0.190836, 0.123117, 0.135766)) ~ (0.0003074861, epsilonF(8))

  property("langermann") = forAll(gen2(0.0, 10.0)) { g =>
    langermann(g) >= -5.1621259
  } && langermann(Sized(2.00299219, 1.006096)) ~ (-5.1621259, epsilonF(6))

  property("leon") = forAll(gen2(-1.2, 1.2)) { g =>
    leon(g) >= 0.0
  } && leon(Sized(1.0, 1.0)) === 0.0

  property("levy3") = forAll(genSized(-10.0, 10.0)) { g =>
    levy3(g) >= 0.0
  } && forAll(genSized(1.0, 1.0)) { g =>
    levy3(g) ~ epsilon
  }

  property("levy5-13") = forAll(gen2(-10.0, 10.0)) { g =>
     levy5(g) >= -176.1375 &&
     levy13(g) >= 0.0
  } && {
     levy5(Sized(-1.3068, -1.4248)) ~ (-176.1375, epsilonF(4)) &&
     levy13(Sized(1.0, 1.0)) ~ epsilon
  }

  property("levyMontalvo2") = forAll(genSized(-5.0, 5.0)) { g =>
    levyMontalvo2(g) >= 0.0
  } && forAll(genSized(1.0, 1.0)) { g =>
    levyMontalvo2(g) ~ epsilon
  }

  property("matyas") = forAll(gen2(-10.0, 10.0)) { g =>
    matyas(g) >= 0.0
  } && matyas(Sized(0.0, 0.0)) === 0.0

  property("maximum") = forAll(genSized(-1000.0, 1000.0)) { g =>
    maximum(g) == g.max &&
    g.exists(_ === maximum(g))
  }

  property("mcCormick") = forAll(gen2D((-1.5, 1.5), (-3.0, 4.0))) { g =>
    mcCormick(g) >= -1.9133
  } && mcCormick(Sized(-0.547, -1.547)) ~ (-1.9133, epsilonF(4))

  property("michalewicz") = forAll(genSized(0.0, Math.PI)) { g =>
    michalewicz(10.0)(g) >= -0.966 * g.size
  } && michalewicz(10.0)(Sized(2.20, 1.57)) ~ (-1.8013, epsilonF(3))

  property("mieleCantrell") = forAll(gen4(-1.0, 1.0)) { g =>
    mieleCantrell(g) >= 0.0
  } && mieleCantrell(Sized(0.0, 1.0, 1.0, 1.0)) === 0.0

  property("minimum") = forAll(genSized(-1000.0, 1000.0)) { g =>
    minimum(g) == g.min &&
    g.exists(_ === minimum(g))
  }

  property("mishra1") = forAll(genSized(0.0, 1.0)) { g =>
    mishra1(g) >= 2.0
  } && forAll(genSized(1.0, 1.0)) { g =>
    mishra1(g) === 2.0
  }

  property("mishra2") = forAll(genSized(0.0, 1.0)) { g =>
    mishra2(g) >= 2.0
  } && forAll(genSized(1.0, 1.0)) { g =>
    mishra2(g) === 2.0
  }

  property("mishra3-4-5-6-8-10") = forAll(gen2(-10.0, 10.0)) { g =>
    mishra3(g) >= -0.19990562 &&
    mishra4(g) >= -0.17767 &&
    mishra5(g) >= -0.119829 &&
    mishra6(g) >= -2.28395 &&
    mishra8(g) >= 0.0 &&
    mishra10(g) >= 0.0
  } && {
    mishra3(Sized(-9.99378322, -9.99918927)) ~ (-0.19990562, epsilonF(7)) &&
    mishra4(Sized(-8.71499636, -9.0533148)) ~ (-0.17767, epsilonF(4)) &&
    mishra5(Sized(-1.98682, -10.0)) ~ (-0.119829, epsilonF(5)) &&
    mishra6(Sized(2.88631, 1.82326)) ~ (-2.28395, epsilonF(5)) &&
    mishra8(Sized(2.0, -3.0)) === 0.0 &&
    mishra10(Sized(0.0, 0.0)) === 0.0 &&
    mishra10(Sized(2.0, 2.0)) === 0.0
  }

  property("mishra7-11") = forAll(genSized(-10.0, 10.0)) { g =>
    mishra7(g) >= 0.0 &&
    mishra11(g) >= 0.0
  } && {
    mishra7(Sized(sqrt(2.0), sqrt(2.0))) ~ 0.0
  } && forAll(genSized(0.0, 0.0)) { g =>
    mishra11(g) === 0.0
  }

  property("mishra9") = forAll(gen3(-10.0, 10.0)) { g =>
    mishra9(g) >= 0.0
  } && {
    mishra9(Sized(1.0, 2.0, 3.0)) == 0.0
  }

  property("multiModal") = forAll(genSized(-10.0, 10.0)) { g =>
    multiModal(g) >= 0.0
  } && forAll(genSized(0.0, 0.0)) { g =>
    multiModal(g) === 0.0
  }

  property("needleEye") = forAll(genSized(-10.0, 10.0)) { g =>
    needleEye(0.0001)(g) >= 0.0
  } && forAll(genConst(0.0001)) { g =>
    needleEye(0.0001)(g) === 0.0
  }

  property("newFunction") = forAll(gen2(-10.0, 10.0)) { g =>
    newFunction1(g) >= -0.184648852475 &&
    newFunction2(g) >= -0.199409030092
  } && {
    newFunction1(Sized(-8.46668984648, -9.99980944557)) ~ (-0.184648852475, epsilonF(6)) &&
    newFunction2(Sized(-9.94114736324, -9.99997128772)) ~ (-0.199409030092, epsilonF(6))
  }

  property("norwegian") = forAll(genSized(-1.1, 1.1)) { g =>
    norwegian(g) >= -1.0
  } && forAll(genConst(1.0)) { g =>
    norwegian(g) === -1.0 ** g.size.toDouble
  }

  property("parsopoulus") = forAll(gen2(-5.0, 5.0)) { g =>
    parsopoulus(g) >= 0.0
  } && {
    val x = List(-1.0, 1.0, -3.0, 3.0, -5.0, 5.0)
    val y = List(0.0, 0.0, -1.0, 1.0, -2.0, 2.0)
    val z = (x zip y).map {
      case (xi, yi) => Sized(xi * (pi / 2.0), yi * pi)
    }
    z.forall(zi => parsopoulus(zi) ~ 0.0)
  }

  property("pathological") = forAll(genSized(-100.0, 100.0)) { g =>
    pathological(g) >= 0.0
  } && pathological(Sized(0.0, 0.0, 0.0)) === 0.0

  property("paviani") = forAll(gen10(2.001, 9.999)) { g =>
    paviani(g) >= -45.7784684040686
  } && {
    val g = Sized(
      9.350266, 9.350266, 9.350266, 9.350266, 9.350266,
      9.350266, 9.350266, 9.350266, 9.350266, 9.350266
    )
    paviani(g) ~ (-45.7784684040686, epsilonF(5))
  }

  property("penalty") = forAll(genSized(-50.0, 50.0)) { g =>
    penalty1(g) >= 0.0
    penalty2(g) >= 0.0
  } && forAll(genSized(1.0, 1.0)) { g =>
    penalty2(g) ~ 0.0
  }

  property("penHolder") = forAll(gen2(-11.0, 11.0)) { g =>
    penHolder(g) >= -0.96354
  } && penHolder(Sized(9.646168, -9.646168)) ~ (-0.96354, epsilonF(5))

  property("periodic") = forAll(genSized(-10.0, 10.0)) { g =>
    periodic(g) >= 0.9
  } && forAll(genConst(0.0)) { g =>
    periodic(g) === 0.9
  }

  property("pinter") = forAll(genSized(-10.0, 10.0)) { g =>
    pinter(g) >= 0.0
  } && forAll(genSized(0.0, 0.0)) { g =>
    pinter(g) === 0.0
  }

  property("plateau") = forAll(genSized(-5.12, 5.12)) { g =>
    plateau(g) >= 30.0
  } && forAll(genConst(0.0)) { g =>
    plateau(g) === 30.0
  }

  property("powell") = forAll(gen4(-4.0, 5.0)) { g =>
    powell(g) >= 0.0
  } && powell(Sized(0.0, 0.0, 0.0, 0.0)) === 0.0

  property("powellSum") = forAll(genSized(-1.0, 1.0)) { g =>
    powellSum(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    powellSum(g) === 0.0
  }

  property("powerSum") = forAll(gen4(0.0, 4.0)) { g =>
    powerSum(g) >= 0.0
  } && powerSum(Sized(1.0, 2.0, 2.0, 3.0)) === 0.0

  property("price1") = forAll(genSized(-500.0, 500.0)) { g =>
    price1(g) >= 0.0
  } && price1(Sized(5.0, -5.0)) === 0.0

  property("price2") = forAll(genSized(-10.0, 10.0)) { g =>
    price2(g) >= 0.9
  } && forAll(genConst(0.0)) { g =>
    price2(g) === 0.9
  }

  property("price3-4") = forAll(gen2(-50.0, 50.0)) { g =>
    price3(g) >= 0.0
    price4(g) >= 0.0
  } && {
    price3(Sized(1.0, 1.0)) ~ 0.0 &&
    price4(Sized(0.0, 0.0)) === 0.0 &&
    price4(Sized(2.0, 4.0)) === 0.0 &&
    price4(Sized(1.464, -2.506)) ~ (0.0, epsilonF(3))
  }

  property("qing") = forAll(genSized(-500.0, 500.0)) { g =>
    qing(g) >= 0.0
  } && qing(Sized(sqrt(1.0), sqrt(2.0), sqrt(3.0))) ~ 0.0

  property("quadratic") = forAll(gen2(-10.0, 10.0)) { g =>
    quadratic(g) >= -3873.7243
  } && {
    quadratic(Sized(0.19388, 0.48513)) ~ (-3873.7243, epsilonF(3))
  }

  property("quadric") = forAll(genSized(-100.0, 100.0)) { g =>
    quadric(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    quadric(g) === 0.0
    quadric(Sized(1.0, 2.0, 3.0)) === 1.0 + 9.0 + 36.0
  }

  property("quintic") = forAll(genSized(-10.0, 10.0)) { g =>
    quintic(g) >= 0.0
  } && {
    quintic(Sized(-1.0, 2.0)) === 0.0 &&
    quintic(Sized(2.0, -1.0)) === 0.0
  }

  property("rana") = rana(Sized(-300.3376, 500.0)) ~ (-500.802160296661, epsilonF(8))

  property("rastrigin") = forAll(genSized(-5.12, 5.12)) { g =>
    rastrigin(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    rastrigin(g) === 0.0
  }

  property("ripple") = forAll(genSized(0.0, 1.0)) { g =>
    ripple1(g) >= -1.1 * g.size
    ripple2(g) >= -1.0 * g.size
  } && forAll(genConst(0.1)) { g =>
    ripple1(g) ~ (-1.1 * g.size, epsilonF(5)) &&
    ripple2(g) === -1.0 * g.size
  }

  property("rosenbrock") = forAll(genSized(-30.0, 30.0)) { g =>
    rosenbrock(g) >= 0.0
  } && forAll(genSized(1.0, 1.0)) { g =>
    rosenbrock(g) === 0.0
  }

  property("rotatedEllipse") = forAll(genSized(-500.0, 500.0)) { g =>
    rotatedEllipse1(g) >= 0.0 &&
    rotatedEllipse2(g) >= 0.0
  } && forAll(genSized(0.0, 0.0)) { g =>
    rotatedEllipse1(g) === 0.0 &&
    rotatedEllipse2(g) === 0.0
  }

  property("salomon") = forAll(genSized(-100.0, 100.0)) { g =>
    salomon(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    salomon(g) === 0.0
  }

  property("sargan") = forAll(genSized(-100.0, 100.0)) { g =>
    sargan(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    sargan(g) === 0.0
  }

  property("schaffer") = forAll(genSized(-100.0, 100.0)) { g =>
    schaffer1(g) >= 0.0 &&
    schaffer2(g) >= 0.0 &&
    schaffer3(g) >= 0.0 &&
    schaffer4(g) >= 0.0
  } && forAll(genSized(0.0, 0.0)) { g =>
    schaffer1(g) === 0.0 &&
    schaffer2(g) === 0.0 &&
    schaffer3(Sized(0.0, 1.253115)) ~ (0.00156685, epsilonF(6)) &&
    schaffer4(Sized(0.0, 1.253115)) ~ (0.29257900, epsilonF(6))
  }

  property("schumerSteiglitz") = forAll(genSized(-100.0, 100.0)) { g =>
    schumerSteiglitz(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    schumerSteiglitz(g) === 0.0
  }

  property("schwefel1") = forAll(genSized(-100.0, 100.0)) { g =>
    schwefel1(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    schwefel1(g) === 0.0
  }

  property("schwefel12") = forAll(genSized(-500.0, 500.0)) { g =>
    schwefel12(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    schwefel12(g) === 0.0
  }

  property("schwefel220") = forAll(genSized(-100.0, 100.0)) { g =>
    schwefel220(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    schwefel220(g) === 0.0
  }

  property("schwefel221") = forAll(genSized(-500.0, 500.0)) { g =>
    schwefel221(g) >= 0.0
  } && forAll(genSized(0.0, 0.0)) { g =>
    schwefel221(g) === 0.0
  }

  property("schwefel222") = forAll(genSized(-500.0, 500.0)) { g =>
    schwefel222(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    schwefel222(g) === 0.0
  }

  property("schwefel223") = forAll(genSized(-10.0, 10.0)) { g =>
    schwefel223(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    schwefel223(g) === 0.0
  }

  property("schwefel225") = forAll(genSized(-10.0, 10.0)) { g =>
    schwefel225(g) >= 0.0
  } && forAll(genSized(1.0, 1.0)) { g =>
    schwefel225(g) === 0.0
  }

  property("schwefel226") = forAll(genSized(-500.0, 500.0)) { g =>
    schwefel226(g) >= 0.0
  } && forAll(genConst(420.968746)) { g =>
    schwefel226(g) ~ (0.0, epsilonF(2))
  }

  property("schwefel236") = forAll(gen2(0.0, 500.0)) { g =>
    schwefel236(g) >= -3456.0
  } && schwefel236(Sized(12.0, 12.0)) === -3456.0

  property("schwefel24") = forAll(genSized(0.0, 10.0)) { g =>
    schwefel24(g) >= 0.0
  } && forAll(genSized(1.0, 1.0)) { g =>
    schwefel24(g) === 0.0
  }

  property("schwefel26") = forAll(gen2(-100.0, 100.0)) { g =>
    schwefel26(g) >= 0.0
  } && schwefel26(Sized(1.0, 3.0)) === 0.0

  property("shekel") = forAll(gen4(0.0, 10.0)) { g =>
    shekel5(g) >= -10.1527 &&
    shekel7(g) >= -10.4028188 &&
    shekel10(g) >= -10.5362837
  } && {
    val g = Sized(4.0, 4.0, 4.0, 4.0)
    shekel5(g)  ~ (-10.1527, epsilonF(4)) &&
    shekel7(g)  ~ (-10.4028188, epsilonF(7)) &&
    shekel10(g) ~ (-10.5362837, epsilonF(7))
  }

  property("shubert") = forAll(gen2(-10.0, 10.0)) { g =>
    shubert1(g) >= -186.7309 &&
    shubert3(g) >= -24.06249 &&
    shubert4(g) >= -29.016015
  } && {
    shubert1(Sized(-7.0835, 4.8580)) ~ (-186.7309, epsilonF(4)) &&
    shubert3(Sized(5.791794, 5.791794)) ~ (-24.062499, epsilonF(6)) &&
    shubert4(Sized(-0.80032121, -7.08350592)) ~ (-29.016015, epsilonF(6))
  }

  property("sineEnvelope") = forAll(gen2(-100.0, 100.0)) { g =>
    sineEnvelope(g) >= 0.0
  } && sineEnvelope(Sized(0.0, 0.0)) === 0.0

  property("sixHumpCamelback") = forAll(gen2(-5.0, 5.0)) { g =>
    sixHumpCamelback(g) >= -1.0316285
  } && {
    sixHumpCamelback(Sized(-0.08983, 0.7126)) ~ (-1.0316285, epsilonF(5)) &&
    sixHumpCamelback(Sized(0.08983, -0.7126)) ~ (-1.0316285, epsilonF(5))
  }

  property("spherical") = forAll(genSized(-100.0, 100.0)) { g =>
    spherical(g) === spherical(g.map(_ * -1)) &&
    spherical(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    spherical(g) === 0.0
  }

  property("step") = forAll(genSized(-100.0, 100.0)) { g =>
    step1(g) >= 0.0 &&
    step2(g) >= 0.0 &&
    step3(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    step1(g) === 0.0 &&
    step2(g) === 0.25 * g.size &&
    step3(g) === 0.0
  }

  property("stretchedVSineWave") = forAll(genSized(-10.0, 10.0)) { g =>
    stretchedVSineWave(g) >= 0.0
  } && forAll(genSized(0.0, 0.0)) { g =>
    stretchedVSineWave(g) === 0.0
  }

  property("styblinksiTang") = forAll(genSized(-5.0, 5.0)) { g =>
    styblinksiTang(g) >= -39.16616570377142 * g.size
  } && forAll(genConst(-2.90353401818596)) { g =>
    styblinksiTang(g) ~ (-39.16616570377142 * g.size, epsilonF(10))
  }

  property("sumSquares") = forAll(genSized(-10.0, 10.0)) { g =>
    sumSquares(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    sumSquares(g) === 0.0
  }

  property("sumDifferentPowers") = forAll(genSized(-1.0, 1.0)) { g =>
    sumDifferentPowers(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    sumDifferentPowers(g) === 0.0
  }

  property("threeHumpCamelback") = forAll(gen2(-5.0, 5.0)) { g =>
    threeHumpCamelback(g) >= 0.0
  } && threeHumpCamelback(Sized(0.0, 0.0)) === 0.0

  property("trecanni") = forAll(gen2(-5.0, 5.0)) { g =>
    trecanni(g) >= 0.0
  } && {
    trecanni(Sized(0.0, 0.0))  === 0.0 &&
    trecanni(Sized(-2.0, 0.0)) === 0.0
  }

  property("trefethen") = forAll(gen2(-10.0, 10.0)) { g =>
    trefethen(g) >= -3.30686865
  } && trefethen(Sized(-0.024403, 0.210612)) ~ (-3.30686865, epsilonF(2))

  property("trid") = forAll(genSized(-20.0, 20.0)) { g =>
    trid(g) >= -50.0
  } && trid(Sized(6.0, 10.0, 12.0, 12.0, 10.0, 6.0)) === -50.0

  property("trigonometric1") = forAll(genSized(0.0, pi)) { g =>
    trigonometric1(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    trigonometric1(g) === 0.0
  }

  property("trigonometric2") = forAll(genSized(-500.0, 500.0)) { g =>
    trigonometric2(g) >= 1.0
  } && forAll(genConst(0.9)) { g =>
    trigonometric2(g) === 1.0
  }

  property("tripod") = forAll(gen2(-100.0, 100.0)) { g =>
    tripod(g) >= 0.0
  } && tripod(Sized(0.0, -50.0)) === 0.0

  property("ursem1") = forAll(gen2D((-2.5, 3.0), (-2.0, 2.0))) { g =>
    ursem1(g) >= -4.8168
  } && ursem1(Sized(1.69714, 0.0)) ~ (-4.8168, epsilonF(4))

  property("ursem3") = forAll(gen2D((-2.0, 2.0), (-1.5, 1.5))) { g =>
    ursem3(g) >= -3.0
  } && ursem3(Sized(0.0, 0.0)) === -3.0

  property("ursem4") = forAll(gen2(-2.0, 2.0)) { g =>
    ursem4(g) >= -1.5
  } && ursem4(Sized(0.0, 0.0)) === -1.5

  property("ursemWaves") = forAll(gen2D((-0.9, 1.2), (-1.2, 1.2))) { g =>
    ursemWaves(g) >= -8.5536
  } && ursemWaves(Sized(1.2, 1.2)) ~ (-8.5536, epsilonF(5))

  property("venterSobiezcczanskiSobieski") = forAll(gen2(-50.0, 50.0)) { g =>
    venterSobiezcczanskiSobieski(g) >= -400.0
  } && venterSobiezcczanskiSobieski(Sized(0.0, 0.0)) === -400.0

  property("vincent") = forAll(genSized(0.25, 10.0)) { g =>
    vincent(g) >= -g.size + 0.0
  } && forAll(genConst(7.70628098)) { g =>
    vincent(g) ~ (-g.size + 0.0, epsilonF(8))
  }

  property("watson") = forAll(gen6(-5.0, 5.0)) { g =>
    watson(g) >= 0.002288
  } && watson(Sized(-0.0158, 1.012, -0.2329, 1.260, -1.513, 0.9928)) ~ (0.002288, epsilonF(4))

  property("wayburnSeader1") = forAll(gen2(-5.0, 5.0)) { g =>
    wayburnSeader1(g) >= 0.0
  } && {
    wayburnSeader1(Sized(1.0, 2.0)) === 0.0 &&
    wayburnSeader1(Sized(1.597, 0.806)) ~ (0.0, epsilonF(3))
  }

  property("wayburnSeader2-3") = forAll(gen2(-500.0, 500.0)) { g =>
    wayburnSeader2(g) >= 0.0 &&
    wayburnSeader3(g) >= 21.349
  } && {
    wayburnSeader2(Sized(0.2, 1.0)) ~ (0.0, epsilonF(5)) &&
    wayburnSeader2(Sized(0.425, 1.0)) ~ (0.0, epsilonF(5)) &&
    wayburnSeader3(Sized(5.611, 6.187)) ~ (21.349, epsilonF(3))
  }

  property("wavy") = forAll(genSized(-pi, pi)) { g =>
    wavy(10.0)(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    wavy(10.0)(g) === 0.0
  }

  property("weierstrass") = forAll(genSized(-0.5, 0.5)) { g =>
    weierstrass(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    weierstrass(g) === 0.0
  }

  property("whitley") = forAll(genSized(-10.24, 10.24)) { g =>
    whitley(g) >= 0.0
  } && forAll(genConst(1.0)) { g =>
    whitley(g) === 0.0
  }

  property("wolfe") = forAll(gen3(0.0, 2.0)) { g =>
    wolfe(g) >= 0.0
  } && wolfe(Sized(0.0, 0.0, 0.0)) === 0.0

  property("wood") = forAll(gen4(-100.0, 100.0)) { g =>
    wood(g) >= 0.0
  } && wood(Sized(1.0, 1.0, 1.0, 1.0)) === 0.0

  property("xinSheYang2") = forAll(genSized(-2.0 * pi, 2.0 * pi)) { g =>
    xinSheYang2(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    xinSheYang2(g) === 0.0
  }

  property("xinSheYang3") = forAll(genSized(-20.0, 20.0)) { g =>
    xinSheYang3(5.0)(g) >= -1.0
  } && forAll(genConst(0.0)) { g =>
    xinSheYang3(5.0)(g) === -1.0
  }

  property("xinSheYang4") = forAll(genSized(-10.0, 10.0)) { g =>
    xinSheYang4(g) >= -1.0
  } && forAll(genConst(0.0)) { g =>
    xinSheYang4(g) === -1.0
  }

  property("yaoLiu4") = forAll(genSized(-10.0, 10.0)) { g =>
    yaoLiu4(g) >= 0.0 &&
    yaoLiu9(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    yaoLiu4(g) === 0.0 &&
    yaoLiu9(g) === 0.0
  }

  property("zakharov") = forAll(genSized(-5.00, 10.0)) { g =>
    zakharov(g) >= 0.0
  } && forAll(genConst(0.0)) { g =>
    zakharov(g) === 0.0
  }

  property("zeroSum") = forAll(genSized(-10.0, 10.0)) { g =>
    zeroSum(g) >= 0.0
  } && {
    zeroSum(Sized(0.0, 0.0, 0.0)) === 0.0 &&
    zeroSum(Sized(1.0, 2.0, -3.0)) === 0.0 &&
    zeroSum(Sized(1.0, 1.0, -2.0)) === 0.0
  }

  property("zettle") = forAll(gen2(-1.0, 5.0)) { g =>
    zettle(g) >= -0.0037912371501199
  } && zettle(Sized(-0.0299, 0.0)) === -0.0037912371501199

  property("zirilli1") = forAll(gen2(-10.0, 10.0)) { g =>
    zirilli1(g) >= -0.3523
  } && zirilli1(Sized(-1.0465, 0.0)) ~ (-0.3523, epsilonF(4))

  property("zirilli2") = forAll(gen2(-500.0, 500.0)) { g =>
    zirilli2(g) >= 0.0
  } && zirilli2(Sized(0.0, 0.0)) === 0.0

}
