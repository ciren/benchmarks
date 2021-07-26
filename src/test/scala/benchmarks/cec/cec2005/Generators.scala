package benchmarks
package cec
package cec2005

import zio._
import zio.test._
import zio.random.Random
import zio.prelude.NonEmptyList

object Generators {

  def genCECSized(min: Double, max: Double): Gen[Random, (NonEmptyList[Double], NonEmptyList[Double], NonEmptyList[Double], NonEmptyList[Double])] = {

    def genWithSize(n: Int) =
      Gen.listOfN(n)(Gen.double(min, max))
        .flatMap(list =>
          if (list.size <= 0) Gen.fromEffect(UIO.die(new IllegalArgumentException("invalid bounds")))
          else Gen.fromEffect(UIO.succeed(NonEmptyList.fromIterable(list.head, list.tail)))
        )

    for {
      a <- genWithSize(2)
      b <- genWithSize(10)
      c <- genWithSize(30)
      d <- genWithSize(50)
    } yield (a, b, c, d)
  }

}
