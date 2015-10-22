# benchmarks

A collection of *n*-dimensional benchmark functions using [non/spire](https://github.com/non/spire)'s numeric types and [scalaz](https://github.com/scalaz/scalaz)'s `Foldable` and `Maybe` types.

All functions have tests for both minimum values as well as known optima.

## Functions

Functions are defined using algebraic properties:

```scala
def rastrigin[F[_]: Foldable1, A: Field : IsReal : Trig : Monoid](x: F[A]) =
  10 * x.length + x.sum(xi => xi ** 2 - 10 * cos(2 * pi * xi))
```

## Examples

Functions can be used with a number of different numerical types:

### Double

```scala
import scalaz.NonEmptyList

val x = NonEmptyList(1.0, 2.0, 3.0)

spherical(x)
// res1: Double = 15.0

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
val b = 2.0 + Jey.h[Double](1)
val x = NonEmptyList(a, b)

spherical(x)
// res5: Jet[Double] = (5.0 + [2.0, 4.0]h)

val z = (a, b)

ackley2(z)
// res7: Jet[Double] = (-191.2527797034299 + [1.7106168652106453, 3.4212337304212905]h)
```
