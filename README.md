# benchmarks

[![Build Status](https://travis-ci.org/cirg-up/benchmarks.svg?branch=master)](https://travis-ci.org/cirg-up/benchmarks)
[![codecov.io](https://codecov.io/github/cirg-up/benchmarks/coverage.svg?branch=master)](https://codecov.io/github/cirg-up/benchmarks?branch=master)

A collection of *n*-dimensional benchmark functions using [non/spire](https://github.com/non/spire)'s numeric types and [scalaz](https://github.com/scalaz/scalaz)'s `Foldable` types.

All functions have tests for both minimum values as well as known optima.

## Functions

Functions are defined using algebraic properties:

```scala
def rastrigin[F[_]: Foldable1, A: Field : IsReal : Trig](x: F[A]) =
  10 * x.length + x.sum(xi => xi ** 2 - 10 * cos(2 * pi * xi))
```

## Examples

Functions can be used with different numerical types:

### Double

```scala
import scalaz.NonEmptyList

val x = NonEmptyList(1.0, 2.0, 3.0)
spherical(x)
// res1: Double = 14.0

val z = (1.0, 2.0)
ackley2(z)
// res2: Double = -191.2527797034299
```

### Jet (Forward AD)
```scala
import spire.math._
import spire.implicits._

implicit val jd = JetDim(2)

val a = 1.0 + Jet.h[Double](0)
val b = 2.0 + Jet.h[Double](1)
val x = NonEmptyList(a, b)

spherical(x)
// res5: Jet[Double] = (5.0 + [2.0, 4.0]h)

val z = (a, b)
ackley2(z)
// res7: Jet[Double] = (-191.2527797034299 + [1.7106168652106453, 3.4212337304212905]h)
```

### Benchmarks

Function | Dimension
-------- | ---------
Absolute | *n*
Ackley | *n*
Adjiman | 2
Alpine1 | *n*
Alpine2 | *n*
Arithmetic Mean | *n*
BartelsConn | 2
Beale | 2
Biggs Exp 2 | 2
Biggs Exp 3 | 3
Biggs Exp 4 | 4
Biggs Exp 5 | 5
Biggs Exp 6 | 6
Bird | 2
Bohachevsky 1 | 2
Bohachevsky 2 | 2
Bohachevsky 3 | 2
Booth | 2
Branin R Cos 2 | 2
Brent | *n*
Brown | *n*
carrom Table | 2
Central Two Peak Trap | 1
Chichinadze | 2
Chung Reynolds | *n*
Cigar | *n*
Colville | 4
Corana | 4
Cosine Mixture | *n*
Cross In Tray | *n*
Cross Leg Table | *n*
Cross Crowned | *n*
Cube | 2
Deb 1 | *n*
Deb 2 | *n*
Decanomial | 2
Deckkers Aarts | 2
Deflected Corrugated Spring | *n*
Devilliers Glasser 1 | 4
Devilliers Glasser 2 | 5
Different Powers | *n*
Discus | *n*
Dixonprice | *n*
Dolan | 5
Dropwave | *n*
Easom | 2
Egg Crate | *n*
Egg Holder | *n*
El Attar Vidyasagar Dutta | 2
Elliptic | *n*
Exponential 1 | *n*
Exponential 2 | 2
Freudenstein Roth | 2
Gear | 4
Giunta | 2
Goldstein Price 1 | 2
Goldstein Price 2 | 2
Griewank | *n*
Hansen | 2
Hartman 3 | 3
Hartman 6 | 6
Helical Valley | 3
Himmelblau | 2
Hosaki | 2
Hyper Ellipsoid | *n*
Hyper Ellipsoid Rotated | *n*
Jennrich Sampson | 2
Judge | 2
Katsuura | *n*
Keane | 2
Kowalik | 4
Langermann | 2
Leon | 2
Levy 3 | *n*
Levy 5 | *n*
Levy 13 | *n*
Levy Montalvo 2 | *n*
Matyas | 2
Maximum | *n*
Michalewicz | *n*
Miele Cantrell | 4
Minimum | *n*
Mishra 1 | *n*
Mishra 2 | *n*
Mishra 3 | 2
Mishra 4 | 2
Mishra 5 | 2
Mishra 6 | 2
Mishra 8 | 2
Mishra 10 | 2
Mishra 7 | *n*
Mishra 11 | *n*
Mishra 9 | 3
Multi Modal | *n*
Needle Eye | *n*
New Function 1 | 2
New Function 2 | 2
Norwegian | *n*
Parsopoulus | 2
Pathological | *n*
Penalty 1 | *n*
Penalty 2 | *n*
Pen Holder | 2
Periodic | *n*
Pinter | *n*
Plateau | *n*
Powell | 4
Powell Sum | *n*
Power Sum | 4
Price 1 | *n*
Price 2 | *n*
Price 3 | 2
Price 4 | 2
Qing | *n*
Quadratic | 2
Quadric | *n*
Quintic | *n*
Rastrigin | *n*
Ripple 1 | *n*
Ripple 2 | *n*
Rosenbrock | *n*
Rotated Ellipse 1 | *n*
Rotated Ellipse 2 | *n*
Salomon | *n*
Sargan | *n*
Schaffer 1 | *n*
Schaffer 2 | *n*
Schaffer 3 | *n*
Schaffer 4 | *n*
Schumer Steiglitz | *n*
Schwefel 1 | *n*
Schwefel 1.2 | *n*
Schwefel 2.20 | *n*
Schwefel 2.21 | *n*
Schwefel 2.22 | *n*
Schwefel 2.23 | *n*
Schwefel 2.25 | *n*
Schwefel 2.26 | *n*
Schwefel 2.36 | 2
Schwefel 2.4 | *n*
Schwefel 2.6 | 2
Shekel 5 | 4
Shekel 7 | 4
Shekel 10 | 4
Shubert 1 | 2
Shubert 3 | 2
Shubert 4 | 2
Sine Envelope | 2
Six Hump Camelback | 2
Spherical | *n*
Step 1 | *n*
Step 2 | *n*
Step 3 | *n*
Stretched V Sinewave | *n*
Styblinksi Tang | *n*
Sum Squares | *n*
Sum Differentpowers | *n*
Three Hump Camelback | 2
Trecanni | 2
Trefethen | 2
Trid | *n*
Trigonometric 1 | *n*
Trigonometric 2 | *n*
Tripod | 2
Ursem4 | 2
Venter Sobiezcczanski Sobieski | 2
Vincent | *n*
Watson | 6
Wayburnseader 1 | 2
Wayburnseader 2 | 2
Wayburnseader 3 | 2
Wavy | *n*
Weierstrass | *n*
Whitley | *n*
Wolfe | 3
Wood | 4
Xin She Yang 2 | *n*
Xin She Yang 3 | *n*
Xin She Yang 4 | *n*
Yao Liu 4 | *n*
Zakharov | *n*
Zerosum | *n*
Zettle | 2
Zirilli1 | 2
Zirilli2 | 2

## Thanks

To [@andyfaff](https://github.com/andyfaff) for providing information on many of the benchmark functions.
