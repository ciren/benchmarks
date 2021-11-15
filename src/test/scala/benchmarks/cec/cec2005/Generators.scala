package benchmarks
package cec
package cec2005

import zio._
import zio.test._
import zio.random.Random
import cilib.NonEmptyVector

object Generators {

  def genCECSized(min: Double, max: Double): Gen[Random, (NonEmptyVector[Double], NonEmptyVector[Double], NonEmptyVector[Double], NonEmptyVector[Double])] = {

    def genWithSize(n: Int): Gen[Random, NonEmptyVector[Double]] =
      Gen.listOfN(n)(Gen.double(min, max))
        .flatMap(list =>
          if (list.size <= 0) Gen.fromEffect(UIO.die(new IllegalArgumentException("invalid bounds")))
          else Gen.fromEffect(UIO.succeed(NonEmptyVector.fromIterable(list.head, list.tail)))
        )

    for {
      a <- genWithSize(2)
      b <- genWithSize(10)
      c <- genWithSize(30)
      d <- genWithSize(50)
    } yield (a, b, c, d)
  }

}
