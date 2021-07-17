package benchmarks

import zio.prelude._
import zio.test._

object Generators {

  // val intervalGen = for {
  //   a <- Gen.choose(-100.0, -1.0) suchThat { _ != 0.0 }
  //   b <- Gen.choose(1.0, 100.0) suchThat { _ != 0.0  }
  // } yield Interval(a, b)

  def nelGen(dim: Int) =
    for {
      head <- Gen.double(-10, 10)
      tail <- Gen.listOfN(dim - 1)(Gen.double(-10, 10))
    } yield NonEmptyList.fromIterable(head, tail)

}
